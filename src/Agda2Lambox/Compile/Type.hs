{-# LANGUAGE OverloadedStrings #-}
module Agda2Lambox.Compile.Type where

import Control.Monad.Reader
import Data.List ( uncons )
import Data.Maybe ( isJust, mapMaybe, fromMaybe )

import Agda.Syntax.Common ( unArg, Arg (Arg) )
import Agda.Syntax.Internal
import Agda.TypeChecking.Substitute ( absBody, absApp )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Debug ( reportSDoc )
import Agda.TypeChecking.Monad.Signature ( canonicalName )
import Agda.TypeChecking.Telescope ( shouldBePi )
import Agda.TypeChecking.Pretty ( prettyTCM, (<+>) )
import Agda.Utils.Monad ( ifM, andM, orM )

import qualified LambdaBox as LBox
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda.Compiler.Backend (HasConstInfo(getConstInfo), AddContext (addContext))
import Agda.Utils (isArity, isFunDef, isTypeAlias, unSpine1, droppedParams)
import Agda.TypeChecking.Reduce ( reduce, reduceDefCopy )
import Agda.TypeChecking.CheckInternal ( infer )


-- | The kind of variables that are locally bound
data VarType = TypeVar Int | Other

-- | λ□ type compilation environment.
data TypeEnv = TypeEnv
  { typeVars   :: Int
    -- ^ How many type variables we have.
  , boundVars  :: Int
    -- ^ Amount of bound variables (including type variables)
  , boundTypes :: [VarType]
    -- ^ Information about the type variables in question.
    -- Should be indexed using de Bruijn indices.
  , insertVars :: Bool
    -- ^ Whether new type variables can be inserted
  , erasedArgs :: [Bool]
    -- ^ Which arrows of the top-level spine take arguments that Agda's erasure removes.
  }

-- | Run a type compilation, given a number of type variables in scope.
runC :: Int -> C a -> CompileM a
runC tvs m = runReaderT m TypeEnv
  { typeVars   = tvs
  , boundVars  = tvs
  , boundTypes = reverse $ TypeVar <$> [0 .. tvs - 1]
  , insertVars = True
  , erasedArgs = []
  }

-- | Increment the number of locally-bound variables.
--   Extend the context with the given type info.
--   The new variable is always considered 'Other'.
underBinder :: Dom Type -> C a -> C a
underBinder v = addContext v . local \e -> e
  { boundVars  = boundVars e + 1
  , boundTypes = Other : boundTypes e
  }

-- | Increment the number of locally-bound variables.
--   Tries to insert a new type variable, if it's allowed.
underTypeVar :: Dom Type -> C a -> C a
underTypeVar b x = do
  shouldInsert <- asks insertVars
  if shouldInsert then
    addContext b $ local (\e@TypeEnv{..} -> e
      { typeVars   = typeVars + 1
      , boundVars  = boundVars + 1
      , boundTypes = TypeVar typeVars : boundTypes
      }) x
  else underBinder b x

-- | Type compilation monad.
type C a = ReaderT TypeEnv CompileM a


-- | Compile constructor arguments' types, given the type variables and the erasure mask.
compileArgs :: Int -> [Bool] -> [Dom Type] -> CompileM [(LBox.Name, LBox.Type)]
compileArgs tvars erased = runC tvars . compileArgsC (erased ++ repeat False)
  where
  compileArgsC :: [Bool] -> [Dom Type] -> C [(LBox.Name, LBox.Type)]
  compileArgsC _ [] = pure []
  compileArgsC ~(e:es) (dom:args) = do
    let name = domToName dom
    liftTCM $ reportSDoc "agda2lambox.compile.arg" 100 $
      "Compiling arg: " <+> prettyTCM dom
    typ <- ifM (liftTCM $ orM [isLogical dom, (e &&) . not <$> isAbsurdFunType dom])
                 (pure LBox.TBox)
                 (compileTypeC $ unDom dom)
    ((name, typ):) <$> underBinder dom (compileArgsC es args)


-- | Compile a top-level type to λ□, lifting out type variables, given its erasure mask.
-- See [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995).
compileTopLevelType :: [Bool] -> Type -> CompileM ([LBox.Name], LBox.Type)
compileTopLevelType erased ty = do
  liftTCM $ reportSDoc "agda2lambox.compile.type" 10 $ "Compiling top level type: " <+> prettyTCM ty
  runC 0 $ withErasedArgs erased $ compileTopLevelTypeC ty

-- | Compile a type that is not on the top-level spine, i.e. no new type variables.
nested :: C a -> C a
nested = local \e -> e { insertVars = False, erasedArgs = [] }

withErasedArgs :: [Bool] -> C a -> C a
withErasedArgs erased = local \e -> e { erasedArgs = erased }

compileTypeC :: Type -> C LBox.Type
compileTypeC = nested . fmap snd . compileTopLevelTypeC

compileElims :: Type -> Args -> C [LBox.Type]
compileElims _ [] = pure []
compileElims ty (x:xs) = do
  liftTCM $ reportSDoc "agda2lambox.compile.elims" 100 $ "Full elim type:" <+> prettyTCM ty
  liftTCM $ reportSDoc "agda2lambox.compile.elims" 100 $ "Current elim:" <+> prettyTCM x
  (a, b) <- liftTCM $ shouldBePi ty
  rest   <- compileElims (absApp b $ unArg x) xs
  -- We compile:
  --   * types (even if erased)
  --   * type variables of a non-sort arity (other type families may be higher-kinded)
  --   * type families given as a λ
  --   * logical arguments to □
  --   * any other argument to 𝕋
  first  <- ifM (liftTCM $ isTypeArg $ unDom a)
              (snd <$> nested (compileTypeTerm $ unArg x)) $
            typeVar (unDom a) (unArg x) >>= \case
              Just i  -> pure $ LBox.TVar i
              Nothing -> ifM (liftTCM $ isLogical a) (pure LBox.TBox) (family (unDom a) (unArg x))
  pure (first : rest)
  where
  -- a declaration drops the arguments of a family parameter (@b x@ ↦ @b@), so
  -- @λ x → B@ stands for @B@ if that compiles without 𝕋 (it does not when @x@ is a type)
  family :: Type -> Term -> C LBox.Type
  family ty t = liftTCM (reduce $ unEl ty) >>= \case
    Pi dom cod | Lam _ body <- t -> underBinder dom $ family (absBody cod) (absBody body)
    Sort{} -> nested (compileTypeTerm t) <&> \(_, t') -> if hasAny t' then LBox.TAny else t'
    _      -> pure LBox.TAny

  hasAny :: LBox.Type -> Bool
  hasAny = \case
    LBox.TAny     -> True
    LBox.TArr s t -> hasAny s || hasAny t
    LBox.TApp s t -> hasAny s || hasAny t
    _             -> False

  isTypeArg :: Type -> TCM Bool
  isTypeArg ty = andM [ isJust . isSort . unEl <$> reduce ty, not <$> isLogical ty ]

  typeVar :: Type -> Term -> C (Maybe Int)
  typeVar ty = \case
    Var n [] -> ifM (liftTCM $ isArity ty) (asks $ tyVarIndex . (!! n) . boundTypes) (pure Nothing)
    _        -> pure Nothing

  tyVarIndex = \case
    TypeVar i -> Just i
    Other     -> Nothing

getTypeVarInfo :: Dom Type -> TCM LBox.TypeVarInfo
getTypeVarInfo typ = do
  let theTyp = unDom typ
  isLg <- isLogical theTyp
  isAr <- isArity theTyp
  let isSt = isJust $ isSort $ unEl theTyp
  pure LBox.TypeVarInfo
    { tvarName      = domToName typ
    , tvarIsArity   = isAr
    , tvarIsLogical = isLg
    , tvarIsSort    = isSt
    }


compileTopLevelTypeC :: Type -> C ([LBox.Name], LBox.Type)
compileTopLevelTypeC typ =
  ifM (liftTCM $ isLogical typ) (pure ([], LBox.TBox)) $
    compileTypeTerm (unEl typ)

-- NOTE(flupe):
--   More or less the algorithm described in the paper (E^T in Fig.2).
compileTypeTerm :: Term -> C ([LBox.Name], LBox.Type)
compileTypeTerm t = do
  liftTCM $ reportSDoc "agda2lambox.compile.type" 200 $ "Compiling type:" <+> prettyTCM t
  case unSpine1 t of
    Sort{}     -> pure ([], LBox.TBox)
    Level{}    -> pure ([], LBox.TBox)
    DontCare{} -> pure ([], LBox.TBox)

    Var n _ -> (!! n) <$> asks boundTypes <&> \case
      TypeVar n -> ([], LBox.TVar n)
      Other     -> ([], LBox.TAny  )

    -- definitions introduced by module application are unfolded
    Def q es -> reduceDefCopy q es >>= \case
      YesReduction _ t' -> compileTypeTerm t'
      NoReduction ()    -> do
        Defn{theDef = def, defType, defArgInfo} <- liftTCM $ getConstInfo q
        isLogicalDef <- liftTCM $ isLogical $ Arg defArgInfo defType
        if
          -- if this def is logical, we ignore it (and its arguments)
          | isLogicalDef -> pure ([], LBox.TBox)

          -- an inductive is applied to its parameters and indices,
          -- unless Agda erases its values (e.g. unit-like records)
          | defIsDataOrRecord def -> ifM (liftTCM $ erasable <$> headInfo q) (pure ([], LBox.TBox)) do
            lift $ requireDef q
            ind <- liftTCM $ toInductive q
            ([],) . foldl' LBox.TApp (LBox.TInd ind)
              <$> compileElims defType (mapMaybe isApplyElim es)

          -- a type-level function that is not an alias is compiled to its reduct,
          -- if any, else to □ if Agda erases the values of all its clauses
          | isFunDef def && not (isTypeAlias def) ->
            liftTCM (reduce $ Def q es) >>= \case
              Def q' _ | q' == q -> ifM (liftTCM $ erasable <$> headInfo q) (pure ([], LBox.TBox)) $
                                      typeScheme q defType es
              t'                 -> compileTypeTerm t'

          -- a type alias is kept, unless it hides arrows of the top-level spine,
          -- which determine type variables and dearging masks (e.g. @incr : Endo Nat@)
          | isTypeAlias def ->
            ifM (asks insertVars) (liftTCM $ reduce $ Def q es) (pure $ Def q es) >>= \case
              t'@Pi{} -> compileTypeTerm t'
              _       -> typeScheme q defType es

          -- otherwise, it must have been compiled as a type scheme,
          -- and therefore is kept with all arguments.
          | otherwise -> typeScheme q defType es

    Pi dom codom -> do
      let domType   = unDom dom
      let codomType = absBody codom

      top <- asks insertVars
      (erased, erasedRest) <- asks $ fromMaybe (False, []) . uncons . erasedArgs
      domIsLogical <- liftTCM $ isLogical dom
      domIsTyVar   <- liftTCM $ andM [ isArity domType, not <$> isLogical domType ]

      -- only non-logical arities are lifted to type variables, irrespective of
      -- their quantity, as in 'inductivePars'
      (vars, ty) <- if not domIsTyVar then do
        domType' <- if domIsLogical || erased then pure LBox.TBox
                                              else compileTypeC domType
        (vars, codomType') <- underBinder dom $ withErasedArgs erasedRest $
          compileTopLevelTypeC codomType
        pure (vars, LBox.TArr domType' codomType')

      else do
        (vars, codomType') <- underTypeVar dom $ withErasedArgs erasedRest $
          compileTopLevelTypeC codomType
        let name = domToName dom
        pure (name : vars, LBox.TArr LBox.TBox codomType')

      -- Agda erases the values of nested function types into □
      pure (vars, case ty of
        LBox.TArr _ LBox.TBox | not top -> LBox.TBox
        _                               -> ty)

    _ -> pure ([], LBox.TAny)

-- | Restore the parameters of an application of a projection(-like) function.
projectionParams :: QName -> Elims -> TCM Elims
projectionParams q es = droppedParams . theDef <$> getConstInfo q >>= \case
  npars | npars > 0, Apply r : _ <- es ->
    unEl <$> (reduce =<< infer (unArg r)) <&> \case
      Def _ ps | Just pars <- allApplyElims (take npars ps), length pars == npars
        -> map Apply pars ++ es
      _ -> es
  _ -> pure es

-- | Compile the application of a type scheme, restoring the parameters of a projection.
typeScheme :: QName -> Type -> Elims -> C ([LBox.Name], LBox.Type)
typeScheme q defType es = do
  es <- liftTCM $ projectionParams q es
  q <- liftTCM $ canonicalName q
  lift $ requireDef q
  let args = mapMaybe isApplyElim es
  kn <- qnameToKName q
  ([],) . foldl' LBox.TApp (LBox.TConst kn)
    <$> compileElims defType args
