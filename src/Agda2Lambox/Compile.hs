{-# LANGUAGE NamedFieldPuns, DataKinds, OverloadedStrings, NondecreasingIndentation #-}
module Agda2Lambox.Compile 
  ( compile
  ) where

import Control.Monad.IO.Class ( liftIO )
import Data.IORef

import Control.Monad.State

import Data.Bifunctor (bimap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Agda.Compiler.Backend
import Agda.Syntax.Internal ( QName )
import Agda.Syntax.Common (Arg(..))
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Monad ( liftTCM, getConstInfo )
import Agda.TypeChecking.Pretty
import Agda.Utils.Monad ( whenM, ifM, unlessM, ifNotM, orM, forM_ )

import Agda.Utils ( hasPragma, isDataOrRecDef, treeless, isArity )

import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Term       ( compileTerm )
import Agda2Lambox.Compile.Function   ( compileFunction )
import Agda2Lambox.Compile.Inductive  ( compileInductive )
import Agda2Lambox.Compile.TypeScheme ( compileTypeScheme )

import LambdaBox.Names
import LambdaBox.Env (GlobalEnv(..), GlobalDecl(..), ConstantBody(..))
import LambdaBox.Type qualified as LamBox

import Agda2Lambox.Compile.Type (compileTopLevelType)
import Data.Foldable (foldrM)
import Agda.Utils.Maybe (catMaybes, mapMaybe, isJust)

-- | Compile the given names to a λ□ environment.
compile :: Target t -> [QName] -> TCM (GlobalEnv t)
compile t qs = do
  items <- topoSort <$> compileLoop (compileDefinition t) qs

  let skipped = flip mapMaybe items \item ->
        case itemValue item of
          Nothing -> Nothing
          Just x  -> Just (item { itemValue = x })

  pure $ GlobalEnv $ flip map skipped \CompiledItem{..} ->
    (qnameToKName itemName, itemValue)


-- TODO(flupe): move this somewhere else
type TopoM a = State (Set QName, [CompiledItem a])

topoSort :: forall a. [CompiledItem a] -> [CompiledItem a]
topoSort defs = snd $ execState (traverse (visit Set.empty) defs) (Set.empty, [])
  where
    items = Map.fromList $ map (\x -> (itemName x, x)) defs

    isMarked :: QName -> TopoM a Bool
    isMarked q = Set.member q <$> gets fst

    push :: CompiledItem a -> TopoM a ()
    push item@CompiledItem{itemName} = modify $ bimap (Set.insert itemName) (item:)

    visit :: Set QName -> CompiledItem a -> TopoM a ()
    visit temp item@CompiledItem{..} = do
      unlessM ((Set.member itemName temp ||) <$> isMarked itemName) do
        let deps = catMaybes $ (`Map.lookup` items) <$> itemDeps
        traverse (visit (Set.insert itemName temp)) deps
        push item


compileDefinition :: Target t -> Definition -> CompileM (Maybe (GlobalDecl t))
compileDefinition target defn@Defn{..} = setCurrentRange defName do
  reportSDoc "agda2lambox.compile" 1 $ "Compiling definition: " <+> prettyTCM defName

  -- we skip logical definitions altogether,
  -- and definitions introduced by module application
  ifM
    (orM [ pure defCopy
         , liftTCM $ isLogical $ Arg defArgInfo defType])
    (pure Nothing) do

  case theDef of
    PrimitiveSort{}    -> pure Nothing
    GeneralizableVar{} -> pure Nothing

    Axiom{} -> do
      typ <- whenTyped target $ compileTopLevelType defType
      pure $ Just $ ConstantDecl $ ConstantBody typ Nothing

    Constructor{conData} -> Nothing <$ requireDef conData

    Function{} -> do
      ifNotM (liftTCM $ isArity defType) (compileFunction target defn) do
        -- it's a type scheme
        case target of
          ToUntyped -> pure Nothing
          -- we only compile it with --typed
          ToTyped   -> Just <$> compileTypeScheme defn

    d | isDataOrRecDef d -> compileInductive target defn

    prim@Primitive{..} -> do
      reportSDoc "agda2lambox.compile" 5 $
        "Found primitive: " <> prettyTCM defName <> "."

      getBuiltinThing (PrimitiveName primName) >>= \case

        -- if it's a primitive, we try to compile it as a regular function
        Just (Prim (PrimFun{})) -> do
          reportSDoc "agda2lambox.compile" 5 $ "Trying to compile it as a function."
          compileFunction target defn

        -- otherwise, we create an axiom
        Nothing -> do
          reportSDoc "agda2lambox.compile" 5 $ "Compiling it as an axiom."
          typ <- whenTyped target $ compileTopLevelType defType
          pure $ Just $ ConstantDecl $ ConstantBody typ Nothing

    _ -> genericError $ "Cannot compile: " <> prettyShow defName
