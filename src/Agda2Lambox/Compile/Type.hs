{-# LANGUAGE LambdaCase, FlexibleInstances, MultiWayIf, NamedFieldPuns #-}
module Agda2Lambox.Compile.Type
  ( compileType
  , compileTopLevelType
  , compileArgs
  ) where


import Control.Category ((>>>))
import Control.Monad.Reader
import Control.Monad ( mapM )
import Data.List ( foldl' )
import Data.Function ( (&) )
import Data.Bifunctor ( second )
import Data.Maybe ( isJust )
import Data.Function ( applyWhen )

import Agda.Syntax.Common ( unArg, Arg (Arg) )
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base ( TCM, MonadTCM (liftTCM), Definition(..))
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Utils.Monad (ifM)

import qualified LambdaBox as LBox
import Agda2Lambox.Compile.Utils ( qnameToKName, isLogical )
import Agda2Lambox.Compile.Monad
import Agda.Compiler.Backend (HasConstInfo(getConstInfo), Definition(Defn), AddContext (addContext))
import Agda.Utils (isDataOrRecDef, getInductiveParams, isArity, maybeUnfoldCopy)
import Agda.TypeChecking.Substitute (absBody, TelV (theCore))
import Agda.TypeChecking.Telescope (telView)


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
  }

-- | Initialize compilation environment with a given number of type variables.
initEnv :: Int -> TypeEnv
initEnv tvs = TypeEnv
  { typeVars   = tvs
  , boundVars  = tvs
  , boundTypes = reverse $ TypeVar <$> [0 .. tvs - 1]
  , insertVars = True
  }

runC :: Int -> C a -> CompileM a
runC tvs m = runReaderT m (initEnv tvs)

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


-- | Compile constructor arguments' types, given a set number of type variables.
compileArgs :: Int -> [Dom Type] -> CompileM [(LBox.Name, LBox.Type)]
compileArgs tvars = runC tvars . compileArgsC
  where
  compileArgsC :: [Dom Type] -> C [(LBox.Name, LBox.Type)]
  compileArgsC [] = pure []
  compileArgsC (dom:args) = do
    let name = maybe LBox.Anon (LBox.Named . prettyShow) $ domName dom
    typ <- ifM (liftTCM $ isLogical dom) (pure LBox.TBox) (compileTypeC $ unDom dom)
    ((name, typ):) <$> underBinder dom (compileArgsC args)


-- | Compile a top-level type to λ□, lifting out type variables.
-- See [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995).
compileTopLevelType :: Type -> CompileM ([LBox.Name], LBox.Type)
compileTopLevelType = runC 0 . compileTopLevelTypeC

-- | Compile a type, given a number of type variables in scope.
--   Will not introduce more type variables.
compileType :: Int -> Type -> CompileM LBox.Type
compileType tvars = runC tvars . compileTypeC

compileTypeC :: Type -> C LBox.Type
compileTypeC = local (\e -> e { insertVars = False }) . fmap snd . compileTopLevelTypeC

compileElims :: Elims -> C [LBox.Type]
compileElims = mapM \case
  Apply a  -> fmap snd $ compileTypeTerm $ unArg a
  Proj{}   -> genericError "type-level projection elim not supported."
  IApply{} -> genericError "type-level cubical path application not supported."

compileTopLevelTypeC :: Type -> C ([LBox.Name], LBox.Type)
compileTopLevelTypeC typ =
  ifM (liftTCM $ isLogical typ) (pure ([], LBox.TBox)) $
    compileTypeTerm (unEl typ)

-- NOTE(flupe): more or less the algorithm described in the paper.
compileTypeTerm :: Term -> C ([LBox.Name], LBox.Type)
compileTypeTerm = \case
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

    -- if it's an inductive, we only apply the parameters
    else if isDataOrRecDef def then
      ([],) . foldl' LBox.TApp (LBox.TConst $ qnameToKName q) 
        <$> compileElims (take (getInductiveParams def) es)

    -- TODO: check if it is a type alias
    --   (if it is, do more or less the same thing as above)

    -- otherwise, we ignore it.
    else pure ([], LBox.TAny)

  Pi dom codom -> do
    let domType   = unDom dom
    let codomType = absBody codom

    domIsLogical <- liftTCM $ isLogical dom
    domIsArity   <- liftTCM $ isArity domType

    -- logical types and non-arities can never be lifted to type variables, we keep going
    if domIsLogical || not domIsArity then do
      domType' <- if domIsLogical then pure LBox.TBox
                                  else compileTypeC domType
      (vars, codomType')
        <- underBinder dom $ compileTopLevelTypeC codomType

      pure (vars, LBox.TArr domType' codomType')

    else do
      (vars, codomType') <- underTypeVar dom $ compileTopLevelTypeC codomType
      let name = maybe LBox.Anon (LBox.Named . prettyShow) $ domName dom
      pure (name : vars, LBox.TArr LBox.TBox codomType')

  _ -> pure ([], LBox.TAny)
