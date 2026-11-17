{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Agda2Lambox.Compile.TypeScheme where

import Agda.Syntax.Internal
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend
import Agda.TypeChecking.Telescope ( telView )
import Agda.TypeChecking.Substitute ( TelV(TelV), absBody )
import Agda.TypeChecking.Functions ( etaExpandClause )

import Agda.Utils ( isTypeAlias )
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Type
import LambdaBox.Env
import LambdaBox.Type qualified as LBox

compileTele :: Tele (Dom Type) -> CompileM [TypeVarInfo]
compileTele EmptyTel = pure []
compileTele (ExtendTel t tel) = do
  tvar <- liftTCM $ getTypeVarInfo t
  rest <- addContext t $ compileTele (absBody tel)
  pure $ tvar : rest

-- | Compile a type scheme, i.e. a function into a sort, to a type alias:
--   its body if it has a single clause whose patterns are variables, else 𝕋.
compileTypeScheme :: Definition -> CompileM (GlobalDecl Typed)
compileTypeScheme Defn{..} = do
  reportSDoc "agda2lambox.compile.typescheme" 5 $ "Compiling type scheme:" <+> prettyTCM defName
  TelV tyargs _ <- telView defType
  tvarInfo <- compileTele tyargs
  case theDef of
    Function{funClauses = [cl]} | isTypeAlias theDef -> do
      -- We eta-expand the clause to maximal arity by inserting variable patterns
      -- and applying the body to variables.
      -- This also lifts all lambdas from the body to variable patterns.
      (_, cl) <- etaExpandClause cl
      let Just body = clauseBody cl
          tel = clauseTel cl
      (_, res) <- addContext (KeepNames tel) $ runC (length tel) $ nested $ compileTypeTerm body
      pure $ TypeAliasDecl $ Just (tvarInfo, res)

    _ -> do
      reportSDoc "agda2lambox.compile.typescheme" 5 "Could not compile type scheme. Defaulting to TAny."
      pure $ TypeAliasDecl $ Just (tvarInfo, LBox.TAny)
