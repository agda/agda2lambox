-- | Makes non-strict `let` bindings strict, thunking those that are not
-- demanded on every path.
--
-- This is needed by *strict* backends such as agda2lambox, since the catch-all
-- machinery of the treeless translation (`updateCatchall`) hoists the body of an
-- overlapping clause above its case split, which is sound only under non-strict
-- evaluation.
--
-- Strict bindings used at most once, and not under a λ, are substituted, so that
-- they are only evaluated on the path that uses them, e.g. the branches of an
-- inlined @if_then_else_@.
module Agda.Utils.ThunkLets (thunkLets) where

import Data.Functor.Identity ( Identity(..) )
import Data.Monoid ( Any(..) )

import Agda.Syntax.Treeless
import Agda.TypeChecking.Substitute
import Agda.Compiler.Treeless.Subst ( Occurs(..), UnderLambda(..), occursIn )

import Agda.Utils ( descendM )

-- | Rewrite non-strict @let x = u in b@ into @let x = λ _ → u in b[x □ / x]@.
thunkLets :: TTerm -> TTerm
thunkLets = \case
  TLet Strict u b
    | linear b' -> applySubst ([go u] ++# idS) b'
    | otherwise -> TLet Strict (go u) b'
    where b' = go b
  TLet NonStrict u b
    -- do not thunk if @u@ is already a value or @b@ evaluates @x@ on every path
    | isValue u' || demands 0 b' -> TLet Strict u' b'
    -- if @b@ occurs at most once, substitute it away to avoid a spurious thunk
    | occursOnce b'              -> applySubst ([u'] ++# idS) b'
    | otherwise                  -> TLet Strict (TLam $ raise 1 u') (force 0 b')
    where
    u' = go u
    b' = go b

  t -> runIdentity $ descendM (Identity . go) t
  where
  go = thunkLets

-- | Values, which are let-bound without a thunk.
isValue :: TTerm -> Bool
isValue = \case
  TVar{}           -> True
  TLam{}           -> True
  TLit{}           -> True
  TCon{}           -> True
  TPrim{}          -> True
  TErased          -> True
  TUnit            -> True
  TSort            -> True
  TApp (TCon _) es -> all isValue es
  _                -> False

-- | Whether variable @0@ is used at most once.
occursOnce :: TTerm -> Bool
occursOnce b = case occursIn 0 b of Occurs n _ _ -> n <= 1

-- | Whether variable @0@ is used at most once, and not under a λ.
linear :: TTerm -> Bool
linear b = case occursIn 0 b of Occurs n (UnderLambda l) _ -> n <= 1 && not (getAny l)

-- | Whether cbv-evaluation of the term forces variable @i@ on every path.
demands :: Int -> TTerm -> Bool
demands i = \case
  TApp (TPrim PSeq) es
    -- seq's forced arguments do not count, since agda2lambox drops them
    | not (null es) -> go i (last es)
  TVar j            -> i == j
  TApp f es         -> go i f || any (go i) es
  TLet s u b        -> (s == Strict && go i u) || go (i + 1) b
  TCase sc _ d alts -> sc == i || (go i d && all (goAlt i) alts)
  TCoerce t         -> go i t
  TError{}          -> True -- unreachable
  _                 -> False
  where
  go = demands

  goAlt :: Int -> TAlt -> Bool
  goAlt i = \case
    TACon _ n b -> go (i + n) b
    TALit _ b   -> go i b
    TAGuard g b -> go i g || go i b

-- | Replace every occurrence of variable @i@ with its forcing application.
force :: Int -> TTerm -> TTerm
force i = \case
  TVar j
    | i == j    -> forced i
    | otherwise -> TVar j
  TApp (TVar j) es
    | i == j -> mkTApp (forced i) (go i <$> es)
  TApp f es  -> TApp (go i f) (go i <$> es)
  TLet s u b -> TLet s (go i u) (go (i + 1) b)
  TLam b     -> TLam (go (i + 1) b)
  TCoerce t  -> TCoerce (go i t)
  TCase sc ct d alts
    -- cannot force case scrutinee variable: introduce fresh let-binding
    | sc == i  -> TLet Strict (forced i) $ go (i + 1) $
                    case raise 1 (TCase sc ct d alts) of
                      TCase _ ct' d' alts' -> TCase 0 ct' d' alts'
                      t -> t
    | otherwise -> TCase sc ct (go i d) (goAlt <$> alts)
  t -> t
  where
  forced :: Int -> TTerm
  forced j = TApp (TVar j) [TUnit]

  go = force

  goAlt :: TAlt -> TAlt
  goAlt = \case
    TAGuard g b -> TAGuard (go i g) (go i b)
    TACon q a b -> TACon q a (go (i + a) b)
    TALit l b   -> TALit l (go i b)
