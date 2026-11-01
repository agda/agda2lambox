{-# LANGUAGE DataKinds, NoNamedFieldPuns, OverloadedStrings #-}
module Agda2Lambox.Compile.TypeScheme where

import Control.Monad.IO.Class ( liftIO )

import Agda.Syntax.Internal
import Agda.Syntax.Common
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend
import Agda.TypeChecking.Telescope ( telView )
import Agda.TypeChecking.Substitute ( TelV(TelV), absBody )
import Agda.TypeChecking.Functions ( etaExpandClause )

import Agda.Utils ( isTypeAlias )
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Type
import LambdaBox.Env
import LambdaBox.Type qualified as LBox

{- If we've reached type scheme compilation, this means the definition:
 - - is a function.
 - - is an arity (i.e has type .. -> .. -> ... -> S where S is a sort)

 The strategy to compile a type scheme is the following:
 1. ensure that it has a single clause
 2. ensure that all the patterns in the clause simply bind variables
 3. we eta-expand the body.
 -}

compileTele :: Tele (Dom Type) -> CompileM [TypeVarInfo]
compileTele EmptyTel = pure []
compileTele (ExtendTel t tel) = do
  tvar <- liftTCM $ getTypeVarInfo t
  rest <- addContext t $ compileTele (absBody tel)
  pure $ tvar : rest

compileTypeScheme :: Definition -> CompileM (GlobalDecl Typed)
compileTypeScheme Defn{..} = do
  reportSDoc "agda2lambox.compile.typescheme" 5 $ "Compiling type scheme:" <+> prettyTCM defName
  TelV tyargs _ <- telView defType
  tvarInfo <- compileTele tyargs
  let Function{..} = theDef
  case funClauses of
    [cl] | isTypeAlias theDef -> do

      -- We eta-expand the clause to maximal arity by inserting variable patterns
      -- and applying the body to variables.
      -- This also lifts all lambdas from the body to variable patterns.
      cl <- etaExpandClause cl

      let
        -- number of parameters introduced in the clause
        nvars     = length $ namedClausePats cl
        Just body = clauseBody cl

      (_, res) <- addContext (KeepNames $ clauseTel cl) $ runCNoVars nvars $ compileTypeTerm body

      pure $ TypeAliasDecl $ Just (tvarInfo, res)

    -- otherwise, we give up compiling the type alias and return TAny
    _ -> do
      reportSDoc "agda2lambox.compile.typescheme" 5 "Could not compile type scheme. Defaulting to TAny."
      pure $ TypeAliasDecl $ Just (tvarInfo, LBox.TAny)
