{-
NOTE(flupe):
Adapted from Agda.Compiler.Treeless.EliminateDefaults, with three differences:
- we also add branches for unreachable defaults (because we need all cases to be exhaustive)
- we copy the default into every missing branch, instead of let-binding it
- we sort the case alts according to the order of constructors for the given inductive
-}

-- | Eliminates case defaults by adding an alternative for all possible
-- constructors. Literal cases are preserved as-is.
module Agda.Utils.EliminateDefaults where

import Control.Monad
import qualified Data.List as List

import Agda.Syntax.Treeless
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Substitute
import Agda.Compiler.Treeless.Subst () -- instance only

import Agda.Utils ( descendM, descendAltM )


eliminateCaseDefaults :: TTerm -> TCM TTerm
eliminateCaseDefaults = \case
  TCase sc ct@CaseInfo{caseType = CTData qn} def alts -> do
    dtCons <- defConstructors . theDef <$> getConstInfo qn

    let missingCons = dtCons List.\\ map aCon alts
    def <- eliminateCaseDefaults def

    -- we produce a new alternative for every missing constructor
    -- whose body is the default body, raised by #args brought in scope
    newAlts <- forM missingCons \con -> do
      Constructor {conArity = ar} <- theDef <$> getConstInfo con
      return $ TACon con ar $ raise ar def

    alts' <- (++ newAlts) <$> mapM (descendAltM eliminateCaseDefaults) alts

    -- then we sort the alts
    let alts'' = flip List.sortOn alts' \alt -> List.elemIndex (aCon alt) dtCons

    return $ TCase sc ct tUnreachable alts''

  -- case on non-data are always exhaustive
  t -> descendM eliminateCaseDefaults t
