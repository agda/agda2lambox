-- | Eta-expansion on constructors to fully-apply them, for treeless syntax.
module Agda.Utils.EtaExpandConstructors (etaExpandConstructors) where

import Data.Bifunctor (second)

import Agda.Syntax.Treeless
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Substitute
import Agda.Compiler.Treeless.Subst () --instance only

import Agda.Utils ( getConArity, descendM )


-- | Check whether a treeless term is a constructor applied to (many) terms.
unSpineCon :: TTerm -> Maybe (QName, [TTerm])
unSpineCon (TCon q)   = Just (q, [])
unSpineCon (TApp u v) = second (++ v) <$> unSpineCon u
unSpineCon _          = Nothing

-- | Eta-expand treeless constructors.
etaExpandConstructors :: TTerm -> TCM TTerm
etaExpandConstructors t | Just (q, args) <- unSpineCon t = do
  arity <- getConArity q
  let nargs = length args

  if nargs >= arity then -- should really be (==), but just in case
    TApp (TCon q) <$> mapM etaExpandConstructors args

  else do
    let nlam = arity - nargs
    exargs <- mapM (etaExpandConstructors . raise nlam) args
    let vars = TVar <$> reverse [0 .. nlam - 1]
    pure $ iterate TLam (TApp (TCon q) $ exargs ++ vars) !! nlam

etaExpandConstructors t = descendM etaExpandConstructors t
