-- | The treeless pipeline of agda2lambox.
module Agda.Utils.Treeless ( treeless ) where

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Treeless ( TTerm, EvaluationStrategy(..) )
import Agda.TypeChecking.Monad ( TCM )
import Agda.Compiler.ToTreeless
import Agda.Compiler.Treeless.Erase ( eraseTerms )
import Agda.Compiler.Treeless.Identity ( detectIdentityFunctions )
import Agda.Compiler.Treeless.NormalizeNames ( normalizeNames )
import Agda.Compiler.Treeless.Uncase ( caseToSeq )

import Agda.Utils.EliminateDefaults
import Agda.Utils.EtaExpandConstructors
import Agda.Utils.ThunkLets

-- | Convert compiled clauses to treeless syntax, or 'Nothing' if inlined.
treeless :: QName -> TCM (Maybe TTerm)
treeless = toTreelessWith (EagerEvaluation, EraseUnused, pipeline)

-- | Agda's default pipeline, but customized:
--    - builtin-translation/simplification/@-pattern-recovery
--    + λ□-specific transformations
pipeline :: Int -> QName -> Pipeline
pipeline v q = Sequential
  [ FixedPoint 5 $ Sequential
    [ compilerPass "erase"  (30 + v) "erasure" $ eraseTerms q
    , compilerPass "uncase" (30 + v) "uncase"  $ const caseToSeq
    ]
  , compilerPass "id"       (30 + v) "identity function detection" $ const (detectIdentityFunctions q)
  , compilerPass "ctors"    (30 + v) "eta-expand constructors"     $ const etaExpandConstructors
  , compilerPass "defaults" (30 + v) "remove default branches"     $ const eliminateCaseDefaults
  , compilerPass "thunks"   (30 + v) "thunk lazy let-bindings"     $ const (pure . thunkLets)
  , compilerPass "names"    (30 + v) "normalize names"             $ const normalizeNames
  ]
