{-# LANGUAGE NamedFieldPuns, DataKinds, OverloadedStrings #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Compile.Function
  ( compileFunction
  ) where

import Control.Monad ( forM, when, filterM, unless )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe, isJust )

import Data.Foldable (toList)
import Agda.Syntax.Abstract.Name ( QName, qnameModule )
import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend ( getConstInfo, funInline, reportSDoc )
import Agda.Syntax.Internal (domName)
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( hasQuantity0 )
import Agda.Utils.Monad (guardWithError, whenM)
import Agda.Utils.Lens ( (^.) )
import Agda.Utils.Treeless ( alwaysInline )

import Agda.Utils ( treeless, pp, isRecordProjection, isArity )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Term ( compileTerm )
import Agda2Lambox.Compile.Type ( compileTopLevelType, compileType )

import LambdaBox qualified as LBox
import LambdaBox.Env
import Agda.TypeChecking.Telescope (telViewUpTo)
import Agda.TypeChecking.Substitute (TelV(theCore, theTel))


-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False


-- | Convert a function body to a Lambdabox term.
compileFunctionBody :: [QName] -> Definition -> CompileM LBox.Term
compileFunctionBody ms Defn{defName, theDef} = do
  Just t <- liftTCM $ treeless defName

  reportSDoc "agda2lambox.compile.function" 10 $
    "treeless body:" <+> pretty t

  compileTerm ms t


-- | Whether to skip a function definition to λ□.
shouldSkipFunction :: Definition -> TCM Bool
shouldSkipFunction def@Defn{theDef} | Function{..} <- theDef
  = do
      -- NOTE(flupe): currently we use this function from Treeless
      --              but it is NOT exported, so once we move back to using Agda's default pipeline
      --              we will have to copy the logic here
      inlined <- alwaysInline (defName def)
      return $ theDef ^. funInline -- inlined (from module application)
                 || hasQuantity0 def -- erased
                 || inlined
shouldSkipFunction _ = pure False

-- | Convert a function definition to a λ□ declaration.
compileFunction :: Target t -> Definition -> CompileM (Maybe (LBox.GlobalDecl t))
compileFunction (t :: Target t) defn = do
  inlined <- liftTCM $ alwaysInline (defName defn)
  if inlined then do
    reportSDoc "agda2lambox.compile.function" 5
      "Function skipped, because either inlined, erased or with-generated."
    pure Nothing
  else do
    let fundef@Function{funMutual = Just mutuals} = theDef defn

    -- all defs in the mutual block
    defs <- liftTCM $ filterM (fmap not . shouldSkipFunction) =<< mapM getConstInfo mutuals

    reportSDoc "agda2lambox.compile.function" 5 $
      "Function mutuals:" <+> prettyTCM (map defName defs)

    unless (all isFunction defs) $
      genericError "Only mutually defined functions are supported."

    -- the mutual functions that we actually compile
    -- (so no with-generated functions, etc...)
    let mnames = map defName defs

    -- (conditionally) compile type of function
    typ <- whenTyped t $ case isRecordProjection fundef of
      Nothing -> compileTopLevelType $ defType defn

      -- if it is a (real) projection, drop the parameters from the type
      Just (recName, _) -> do
        Record{recPars} <- fmap theDef $ liftTCM $ getConstInfo recName
        projTel <- telViewUpTo recPars $ defType defn
        projType <- theCore <$> (telViewUpTo recPars $ defType defn)
        let names  = map domName $ toList $ theTel projTel
            pnames = map (maybe LBox.Anon (LBox.Named . sanitize . prettyShow)) names
        (pnames,) <$> compileType recPars projType

      -- TODO(flupe): ^ take care of projection-like functions
      --                they should be eta-expanded somehow,
      --                OR treated like projections

    let builder :: LBox.Term -> Maybe (LBox.GlobalDecl t)
        builder = Just . ConstantDecl . ConstantBody typ . Just

    -- if the function is not recursive, just compile the body
    if null defs then builder <$> compileFunctionBody [] defn

    -- otherwise, take fixpoint
    else do
      let k = fromMaybe 0 $ elemIndex (defName defn) mnames

      builder . flip LBox.LFix k <$>
        forM defs \def@Defn{defName} -> do
          body <- compileFunctionBody mnames def >>= \case
            l@LBox.LLambda{} -> pure l
            LBox.LBox        -> pure $ LBox.LLambda LBox.Anon LBox.LBox
            _                -> genericError "Fixpoint body must be Lambda."
          return LBox.Def
            { dName = qnameToName defName
            , dBody = body
            , dArgs = 0
            }
