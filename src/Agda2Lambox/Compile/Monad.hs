{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , requireDef
  , runCompile
  , compileLoop
  , genericError
  , CompiledItem(..)
  ) where

import Control.Monad ( unless )
import Control.Monad.State
import Data.Bifunctor ( bimap )
import Data.Set ( Set )
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Maybe ( catMaybes )

import Agda.Syntax.Abstract (QName)
import Agda.Compiler.Backend (getConstInfo, PureTCM, HasConstInfo, HasBuiltins, canonicalName)
import Agda.TypeChecking.Monad (MonadDebug, MonadTrace, MonadAddContext)
import Agda.TypeChecking.Monad.Debug (reportSDoc)
import Agda.TypeChecking.Monad.Base
import Agda.Utils.Monad ( unlessM )
import Agda.TypeChecking.Pretty
import Agda.Syntax.Common.Pretty qualified as P
import Control.Monad.Error.Class (MonadError)

-- | Backend compilation state.
data CompileState = CompileState
  { seenDefs     :: Set QName
    -- ^ Names that we have seen, either already compiled or in the stack.
  , compileStack :: [QName]
    -- ^ Compilation stack.
  , requiredDefs :: Set QName
    -- ^ (Locally) required definitions.
  }

-- | Backend compilation monad.
newtype CompileM a = Compile (StateT CompileState TCM a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadIO, MonadFail, MonadDebug, ReadTCState, MonadTrace)
  deriving newtype (MonadError TCErr, MonadTCEnv, MonadTCState, HasOptions, MonadTCM)
  deriving newtype (MonadAddContext, MonadReduce, HasConstInfo, HasBuiltins, MonadFileId, PureTCM)

-- | Backend-specific errors.
genericError :: MonadTCError m => String -> m a
genericError = typeError . CustomBackendError "agda2lambox" . P.text

-- | Require a definition to be compiled.
requireDef :: QName -> CompileM ()
requireDef q = Compile $ do
  q <- liftTCM $ canonicalName q

  -- add name to the required list
  modify \ s -> s { requiredDefs = Set.insert q (requiredDefs s) }

  seen <- gets seenDefs

  -- a name is only added to the queue if we haven't seen it yet
  unless (Set.member q seen) do

    reportSDoc "agda2lambox.compile.require" 10 $
      "Requiring definition:" <+> prettyTCM q

    modify \ s -> s
      { seenDefs     = Set.insert q seen
      , compileStack = q : compileStack s
      }

-- | Get the next qname to be compiled, if there is one.
nextUnit :: CompileM (Maybe QName)
nextUnit = Compile $
  gets compileStack >>= \case
    []   -> pure Nothing
    q:qs -> Just q <$ modify \ s -> s {compileStack = qs}

-- | Record the required definitions of a compilation unit.
trackDeps :: CompileM a -> CompileM (a, [QName])
trackDeps (Compile c) = Compile do
  modify \s -> s {requiredDefs = Set.empty}
  x <- c
  deps <- gets requiredDefs
  pure (x, Set.toList deps)

-- | Run a compile action in 'TCM'.
runCompile :: CompileM a -> TCM a
runCompile (Compile c) = evalStateT c $ CompileState Set.empty [] Set.empty

-- | Run the processing function as long as there are names to be compiled.
--  Returns a list of compiled items, (topologically) sorted by dependency order.
--  This means that whenever @A@ depends on @B@, @A@ will appear before @B@ in the list.
compileLoop
  :: forall a. (Definition -> CompileM (Maybe a))
             -- ^ The compilation function
  -> [QName] -- ^ Initial names to compile
  -> CompileM [CompiledItem a]
compileLoop step names = do
  mapM_ requireDef names
  topoSort <$> loop
  where
  loop :: CompileM [CompiledItem (Maybe a)]
  loop = nextUnit >>= \case
    Nothing -> pure []
    Just q  -> do
      (mr, deps) <- trackDeps . step =<< liftTCM (getConstInfo q)
      (CompiledItem q deps mr:) <$> loop

-- * Compilation items and topological sort

-- | Named compilation item, with a set of dependencies.
data CompiledItem a = CompiledItem
  { itemName  :: QName
  , itemDeps  :: [QName]
  , itemValue :: a
  }

-- | Stateful monad for the topological sort.
-- State contains the list of items that have been permanently inserted,
-- along with their names.
type TopoM a = State (Set QName, [CompiledItem a])

-- | Topological sort of compiled items, based on dependencies.
-- Skipped items are required for dependency analysis, as they
-- can transively force ordering
-- (e.g constructors are skipped but force compilation of their datatype).
-- In the end, we get a list of items that are effectively compiled.
topoSort :: forall a. [CompiledItem (Maybe a)] -> [CompiledItem a]
topoSort defs = snd $ execState (mapM_ (visit Set.empty) defs) (Set.empty, [])
  where
    items = Map.fromList $ map (\x -> (itemName x, x)) defs

    -- | Whether an item has been permanently inserted already
    isMarked :: QName -> TopoM a Bool
    isMarked q = Set.member q <$> gets fst

    push :: CompiledItem (Maybe a) -> TopoM a ()
    push item@CompiledItem{itemName, itemValue}
      | Just value <- itemValue
      = modify $ bimap (Set.insert itemName) (item {itemValue = value}:)
      | otherwise = pure ()

    -- NB: an item that is already temporarily marked means a cycle in the graph,
    --     which should never happen; we then pick an arbitrary order.
    visit :: Set QName -> CompiledItem (Maybe a) -> TopoM a ()
    visit temp item@CompiledItem{..} =
      unlessM ((Set.member itemName temp ||) <$> isMarked itemName) do
        mapM_ (visit (Set.insert itemName temp)) $ catMaybes $ (`Map.lookup` items) <$> itemDeps
        push item
