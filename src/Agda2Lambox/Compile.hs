{-# LANGUAGE NamedFieldPuns, DataKinds, OverloadedStrings, NondecreasingIndentation #-}
module Agda2Lambox.Compile 
  ( compile
  ) where

import Control.Monad.IO.Class ( liftIO )
import Data.IORef

import Control.Monad.State

import Agda.Compiler.Backend
import Agda.Syntax.Internal ( QName )
import Agda.Syntax.Common (Arg(..), IsOpaque (TransparentDef))
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Monad ( liftTCM, getConstInfo )
import Agda.TypeChecking.Pretty
import Agda.Utils.Monad ( ifM, ifNotM, orM )
import Agda.Utils.SmallSet qualified as SmallSet

import Agda.Utils ( hasPragma, isDataOrRecDef, treeless, isArity )

import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Term       ( compileTerm )
import Agda2Lambox.Compile.Function   ( compileFunction )
import Agda2Lambox.Compile.Inductive  ( compileInductive )
import Agda2Lambox.Compile.TypeScheme ( compileTypeScheme )
import Agda2Lambox.Compile.Type       ( compileTopLevelType )

import LambdaBox ( emptyName, emptyDecl )
import LambdaBox.Names
import LambdaBox.Env (GlobalEnv(..), GlobalDecl(..), ConstantBody(..))
import LambdaBox.Term (Term(LBox))



-- | Compile the given names into to a λ□ environment.
compile :: Target t -> [QName] -> CompileM (GlobalEnv t)
compile t qs = do
  items <- compileLoop (compileDefinition t) qs
  pure $ GlobalEnv $ map itemToEntry items ++ [(emptyName, emptyDecl t)]
  where
    itemToEntry :: CompiledItem (GlobalDecl t) -> (KerName, GlobalDecl t)
    itemToEntry CompiledItem{..} = (qnameToKName itemName, itemValue)


compileDefinition :: Target t -> Definition -> CompileM (Maybe (GlobalDecl t))
compileDefinition target defn@Defn{..} = setCurrentRange defName do
  reportSDoc "agda2lambox.compile" 1 $ "Compiling definition: " <+> prettyTCM defName

  -- we skip definitions introduced by module application

  if defCopy then pure Nothing else do

  typ <- whenTyped target $ compileTopLevelType defType

  -- TODO: check that we indeed don't compile defs marked with @0
  --       especially record projections for erased fields
  -- logical definitions are immediately compiled to □
  ifM (liftTCM $ isLogical $ Arg defArgInfo defType)
     (pure $ Just $ ConstantDecl $ ConstantBody typ $ Just LBox) do

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

    Primitive{..} -> do
      reportSDoc "agda2lambox.compile" 5 $
        "Found primitive: " <> prettyTCM defName

      getBuiltinThing (PrimitiveName primName) >>= \case

        -- if it's a primitive with an actual implementation
        -- we try to convert it to a function, manually
        Just (Prim (PrimFun{})) -> do
          reportSDoc "agda2lambox.compile" 5 $
            "It's a builtin, converting it to a function."
          let
            defn' = defn
              { theDef = Function
                { funClauses    = primClauses
                , funCompiled   = primCompiled
                , funSplitTree  = Nothing
                , funTreeless   = Nothing
                , funCovering   = [] -- NOTE(flupe): should we try computing this?
                , funInv        = primInv
                , funMutual     = Just [defName]
                , funProjection = Left NeverProjection
                , funFlags      = SmallSet.empty
                , funTerminates = Just True
                , funExtLam     = Nothing
                , funWith       = Nothing
                , funIsKanOp    = Nothing
                , funOpaque     = TransparentDef
                }
              }

          liftTCM $ modifyGlobalDefinition defName $ const defn'

          -- and then we compile it as a regular function
          compileFunction target defn'

        -- otherwise, compiling it as an axiom
        _ -> do
          reportSDoc "agda2lambox.compile" 5 $ "Compiling it to an axiom."
          typ <- whenTyped target $ compileTopLevelType defType
          pure $ Just $ ConstantDecl $ ConstantBody typ Nothing

    _ -> genericError $ "Cannot compile: " <> prettyShow defName
