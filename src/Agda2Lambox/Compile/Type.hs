{-# LANGUAGE OverloadedStrings #-}
module Agda2Lambox.Compile.Type where

import Control.Monad.Reader
import Data.List ( uncons )
import Data.Functor ( (<&>) )
import Data.Maybe ( isJust, mapMaybe, fromMaybe )

import Agda.Syntax.Common ( unArg, Arg (Arg) )
import Agda.Syntax.Internal
import Agda.TypeChecking.Substitute ( absBody, absApp )
import Agda.TypeChecking.Monad.Base hiding ( initEnv )
import Agda.TypeChecking.Monad.Debug ( reportSDoc )
import Agda.TypeChecking.Monad.Signature ( canonicalName )
import Agda.TypeChecking.Telescope ( shouldBePi )
import Agda.TypeChecking.Pretty ( prettyTCM, (<+>) )
import Agda.Utils.Monad ( ifM, andM )
import Agda.Utils ( unSpine1 )

import qualified LambdaBox as LBox
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda.Compiler.Backend (HasConstInfo(getConstInfo), AddContext (addContext))
import Agda.Utils (isDataOrRecDef, isArity, maybeUnfoldCopy, isFunDef, isTypeAlias)
import Agda.TypeChecking.Reduce ( reduce )
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

-- | Initialize compilation environment with a given number of type variables.
initEnv :: Int -> TypeEnv
initEnv tvs = TypeEnv
  { typeVars   = tvs
  , boundVars  = tvs
  , boundTypes = reverse $ TypeVar <$> [0 .. tvs - 1]
  , insertVars = True
  , erasedArgs = []
  }

runC :: Int -> C a -> CompileM a
runC tvs m = runReaderT m (initEnv tvs)

runCNoVars :: Int -> C a -> CompileM a
runCNoVars tvs m = runReaderT m ((initEnv tvs) { insertVars = False })

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
    typ <- ifM ((e ||) <$> liftTCM (isLogical dom))
                 (pure LBox.TBox)
                 (compileTypeC $ unDom dom)
    ((name, typ):) <$> underBinder dom (compileArgsC es args)


-- | Compile a top-level type to λ□, lifting out type variables, given its erasure mask.
-- See [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995).
compileTopLevelType :: [Bool] -> Type -> CompileM ([LBox.Name], LBox.Type)
compileTopLevelType erased ty = do
  liftTCM $ reportSDoc "agda2lambox.compile.type" 10 $ "Compiling top level type: " <+> prettyTCM ty
  runC 0 $ withErasedArgs erased $ compileTopLevelTypeC ty

-- | Compile a type, given a number of type variables in scope.
--   Will not introduce more type variables.
compileType :: Int -> Type -> CompileM LBox.Type
compileType tvars ty = do
  liftTCM $ reportSDoc "agda2lambox.compile.type" 10 $ "Compiling type: " <+> prettyTCM ty
  liftTCM $ reportSDoc "agda2lambox.compile.type" 10 $ "With vars: " <+> prettyTCM tvars
  runC tvars $ compileTypeC ty

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
  -- b <- piApplyM ty x
  rest   <- compileElims (absApp b $ unArg x) xs
  -- types are compiled (even if erased)
  -- type variables of a non-sort arity are compiled (other type families may be higher-kinded)
  -- logical arguments are □, and any other argument is 𝕋
  first  <- ifM (liftTCM $ isTypeArg $ unDom a)
              (snd <$> nested (compileTypeTerm $ unArg x)) $
            typeVar (unDom a) (unArg x) >>= \case
              Just i  -> pure $ LBox.TVar i
              Nothing -> ifM (liftTCM $ isLogical a) (pure LBox.TBox) (pure LBox.TAny)
  pure (first : rest)
  where
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

    Var n _ -> do
      -- NOTE(flupe): I think we should distinguish b/w "other" and logical variables
      (!! n) <$> asks boundTypes >>= \case
        TypeVar n -> pure ([], LBox.TVar n)
        Other     -> pure ([], LBox.TAny  )

    Def q es -> maybeUnfoldCopy q es compileTypeTerm \q es -> do
      Defn{theDef = def, defType, defArgInfo, defName} <- liftTCM $ getConstInfo q

      isLogicalDef <- liftTCM $ isLogical $ Arg defArgInfo defType

      -- if this def is logical, we ignore it (and its arguments)
      if isLogicalDef then pure ([], LBox.TBox)

      -- an inductive is applied to its parameters and indices,
      -- unless Agda erases its values (e.g. unit-like records)
      else if isDataOrRecDef def then ifM (liftTCM $ isErasedData q) (pure ([], LBox.TBox)) do
        lift $ requireDef q
        ind <- liftTCM $ toInductive q
        let args = mapMaybe isApplyElim es
        ([],) . foldl' LBox.TApp (LBox.TInd ind)
          <$> compileElims defType args

      -- a type-level function that is not an alias is compiled to its reduct, if any
      else if isFunDef def && not (isTypeAlias def) then
        liftTCM (reduce $ Def q es) >>= \case
          Def q' _ | q' == q -> typeScheme q defType es
          t'                 -> compileTypeTerm t'

      -- a type alias is kept, unless it hides arrows of the top-level spine,
      -- which determine type variables and dearging masks (e.g. @incr : Endo Nat@)
      else if isTypeAlias def then
        ifM (asks insertVars) (liftTCM $ reduce $ Def q es) (pure $ Def q es) >>= \case
          t'@Pi{} -> compileTypeTerm t'
          _       -> typeScheme q defType es

      -- otherwise, it must have been compiled as a type scheme,
      -- and therefore is kept with all arguments.

      -- TODO(flupe): check whether indeed the fallback is always compiled to a typescheme.
      --              what about non-logical defs that are not arities or w/?
      -- TODO(flupe): possibly merge the logic with the above for datatypes
      else typeScheme q defType es

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

      -- Agda erases the values of nested function types into □ or out of an empty type
      erasedPi <- if top then pure False else case ty of
        LBox.TArr _ LBox.TBox -> pure True
        _                     -> liftTCM $ isEmptyType domType
      pure (vars, if erasedPi then LBox.TBox else ty)

    _ -> pure ([], LBox.TAny)

-- | Restore the parameters of an application of a projection(-like) function.
projectionParams :: QName -> Elims -> TCM Elims
projectionParams q es = theDef <$> getConstInfo q >>= \case
  Function{funProjection = Right Projection{projIndex}}
    | npars <- projIndex - 1, npars > 0, Apply r : _ <- es ->
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
