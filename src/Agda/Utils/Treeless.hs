-- | The treeless pipeline of agda2lambox.
module Agda.Utils.Treeless ( treeless ) where

import Data.Map qualified as Map

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Internal ( namedClausePats )
import Agda.Syntax.Builtin ( someBuiltin, builtinNatMinus, builtinNatDivSucAux, builtinNatModSucAux )
import Agda.Syntax.Treeless ( TTerm(..), EvaluationStrategy(..), tUnreachable )
import Agda.TypeChecking.Monad
import Agda.Compiler.ToTreeless
import Agda.Compiler.Treeless.Erase ( eraseTerms )
import Agda.Compiler.Treeless.Identity ( detectIdentityFunctions )
import Agda.Compiler.Treeless.NormalizeNames ( normalizeNames )
import Agda.Compiler.Treeless.Simplify ( simplifyTTerm )
import Agda.Compiler.Treeless.Uncase ( caseToSeq )

import Agda.Utils.EliminateDefaults
import Agda.Utils.EtaExpandConstructors
import Agda.Utils.ThunkLets

import Agda2Lambox.Compile.Target ( Target(..) )

-- | Convert compiled clauses to treeless syntax, or 'Nothing' if inlined.
treeless :: Target t -> QName -> TCM (Maybe TTerm)
treeless t = toTreelessWith (EagerEvaluation, EraseUnused, pipeline t)

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
  , compilerPass "ctors"    (30 + v) "eta-expand constructors"     $ const etaExpandConstructors
  , compilerPass "defaults" (30 + v) "remove default branches"     $ const eliminateCaseDefaults
  , compilerPass "thunks"   (30 + v) "thunk lazy let-bindings"     $ const (pure . thunkLets)
  , compilerPass "names"    (30 + v) "normalize names"             $ const normalizeNames
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
