{-# LANGUAGE OverloadedStrings #-}
-- | The treeless pipeline of agda2lambox.
module Agda.Utils.Treeless ( treeless, unfoldSelfProjections ) where

import Control.Monad ( forM, forM_ )
import Data.Map qualified as Map
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Any(..) )

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic ( TermLike(..) )
import Agda.Syntax.Treeless ( TTerm(..), EvaluationStrategy(..), tUnreachable )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty ( prettyTCM, (<+>) )
import Agda.TypeChecking.Patterns.Match ( Match(..), matchCopatterns, buildSubstitution )
import Agda.TypeChecking.Substitute ( applySubst, applyE )
import Agda.Utils.Impossible ( impossible )
import Agda.Utils.Size ( size )
import Agda.Compiler.ToTreeless
import Agda.Compiler.Treeless.Erase ( eraseTerms )
import Agda.Compiler.Treeless.Identity ( detectIdentityFunctions )
import Agda.Compiler.Treeless.NormalizeNames ( normalizeNames )
import Agda.Compiler.Treeless.Simplify ( simplifyTTerm )
import Agda.Compiler.Treeless.Uncase ( caseToSeq )

import Agda.Utils.EliminateDefaults
import Agda.Utils.ThunkLets

import Agda2Lambox.Compile.Target ( Target(..) )

-- | Convert compiled clauses to treeless syntax, or 'Nothing' if inlined.
treeless :: Target t -> QName -> TCM (Maybe TTerm)
treeless t q = traverse lower =<< toTreelessWith (EagerEvaluation, EraseUnused, pipeline t) q
  where
  -- | Passes on the final code of a definition, outside the cached output of
  -- 'pipeline' that the treeless translation inlines into callers. Otherwise,
  -- thunked catch-alls would be subsequently inlined by the simplifier.
  lower :: TTerm -> TCM TTerm
  lower = normalizeNames . thunkLets

-- | In the clauses of every recursive function in scope, unfold projections of calls into its
--   recursive group, through the matching clause when that clause is not recursive itself.
--
--   E.g. unification solves the implicit argument of @of@ in
--   @proof (map′ f g (true because p)) = of (f (invert p))@ by @does (map′ f g (true because p))@,
--   whose strict evaluation evaluates the @proof@ field again; the treeless translation
--   normalises clause bodies, but only unfolds copattern definitions that are not recursive.
--
--   Runs before any treeless translation, since the latter caches the code of every callee.
unfoldSelfProjections :: TCM ()
unfoldSelfProjections = do
  defs <- (<>) <$> useTC (stSignature . sigDefinitions) <*> useTC (stImports . sigDefinitions)
  forM_ defs \case
    Defn{defName = q, theDef = Function{funClauses = cls, funMutual = Just grp@(_:_)}}
      | getAny $ foldTerm (Any . isSelfProj grp) (map clauseBody cls) -> do
        cls' <- forM cls \cl -> (\b -> cl{clauseBody = b}) <$> traverseTermM (unfold grp) (clauseBody cl)
        reportSDoc "agda2lambox.selfproj" 10 $ "unfolding self-projections in" <+> prettyTCM q
        modifyGlobalDefinition q $ updateTheDef \d -> d{funClauses = cls', funTreeless = Nothing}
    _ -> pure ()
  where
  isSelfProj grp = \case
    Def g es -> g `elem` grp && any (\case Proj{} -> True; _ -> False) es
    _        -> False

  unfold grp v
    | Def g es <- v, isSelfProj grp v = fromMaybe v <$> (firstMatch es . defClauses =<< getConstInfo g)
    | otherwise = pure v

  firstMatch _ [] = pure Nothing
  firstMatch es (cl : cls)
    | length es0 < n = pure Nothing
    | otherwise = fst <$> onlyReduceProjections (matchCopatterns ps es0) >>= \case
        Yes _ vs | not (couldBeRecursive $ clauseRecursive cl), Just b <- clauseBody cl ->
          pure $ Just $ applySubst (buildSubstitution impossible (size $ clauseTel cl) vs) b `applyE` es1
        No{} -> firstMatch es cls
        _    -> pure Nothing
    where
    ps         = namedClausePats cl
    n          = length ps
    (es0, es1) = splitAt n es

-- | Agda's default pipeline, but customized:
--    - builtin-translation/@-pattern-recovery
--    + λ□-specific transformations
pipeline :: Target t -> Int -> QName -> Pipeline
pipeline t v q
  | ToTyped <- t
  , isAbsurdLambdaName q
  = compilerPass "absurd" (30 + v) "absurd lambda" \ _ _ -> absurdLambda q
  | otherwise
  = Sequential
  [ FixedPoint 5 $ Sequential
    [ compilerPass "simpl"  (30 + v) "simplification" simplify
    , compilerPass "erase"  (30 + v) "erasure" $ eraseTerms q
    , compilerPass "uncase" (30 + v) "uncase"  $ const caseToSeq
    ]
  , compilerPass "id"       (30 + v) "identity function detection" $ const (detectIdentityFunctions q)
  , compilerPass "defaults" (30 + v) "remove default branches"     $ const eliminateCaseDefaults
  ]

-- | Agda's simplifier, hiding the builtins it would turn into primitives
--   (@_-_@ to 'PSub', @div-helper@ to 'PQuot', @mod-helper@ to 'PRem').
simplify :: EvaluationStrategy -> TTerm -> TCM TTerm
simplify eval t = localTCState do
  modifyTCLens stLocalBuiltins hide
  modifyTCLens stImportedBuiltins hide
  simplifyTTerm eval t
  where
  hide m = foldr (Map.delete . someBuiltin) m
    [builtinNatMinus, builtinNatDivSucAux, builtinNatModSucAux]

-- | An absurd lambda: a lambda per pattern with an unreachable body.
absurdLambda :: QName -> TCM TTerm
absurdLambda q = theDef <$> getConstInfo q >>= \case
  Function{funClauses = cl : _} -> pure $ iterate TLam tUnreachable !! length (namedClausePats cl)
  _ -> pure tUnreachable
