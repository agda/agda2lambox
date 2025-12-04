{-# LANGUAGE DataKinds, NoNamedFieldPuns, OverloadedStrings #-}
module Agda2Lambox.Compile.TypeScheme where

import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( isJust )

import Agda.Syntax.Internal
import Agda.Syntax.Common
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend
import Agda.TypeChecking.Telescope ( telView )
import Agda.TypeChecking.Substitute ( TelV(TelV), absBody )
import Agda.TypeChecking.Functions ( etaExpandClause )

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

-- NOTE(flupe): may be fine with dot patterns or something, let's not care right now
-- | Checks that all the patterns of a clause are single-variables
onlyVarsPat :: NAPs -> Bool
onlyVarsPat =
  all \(namedThing . unArg -> x) ->
    case x of
      VarP{} -> True
      _      -> False

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
  let Function{..} = theDef
  case funClauses of
    [cl] | onlyVarsPat (namedClausePats cl)
         , isJust (clauseBody cl) -> do
      cl <- etaExpandClause cl
      addContext (KeepNames $ clauseTel cl) do
        -- retrieve the telescope of arguments that have not been introduced yet
        let
          -- number of parameters introduced in the clause
          nvars = length $ namedClausePats cl
          args = drop nvars $ telToList tyargs

          -- we traverse explicit lambdas
          underLams :: Term -> [Dom (ArgName, Type)] -> CompileM LBox.Type
          underLams (Lam ai t) (dom:rest) =
            underAbstraction (snd <$> dom) t \body ->
              underLams body rest
          underLams t args =
            fmap snd $ runCNoVars nvars $ compileTypeTerm t

          Just body = clauseBody cl

        res      <- underLams body args
        tvarInfo <- compileTele tyargs

        pure $ TypeAliasDecl $ Just (tvarInfo, res)

    -- if there isn't exactly one clause, we give up
    -- compiling the type alias and return TAny
    _ -> do
      reportSDoc "agda2lambox.compile.typescheme" 5 "Could not compile type scheme. Defaulting to TAny."
      pure $ TypeAliasDecl $ Just ([], LBox.TAny)
