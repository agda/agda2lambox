-- | Thunks `let` bindings that are not demanded on every path.
--
-- This is needed by *strict* backends such as agda2lambox, since some
-- compiler-generated let-bindings are assuming lazy/call-by-name semantics, e.g.
-- `updateCatchall` of the treeless pipeline hoists the body of an overlapping
-- clause above its case split, which is sound only under non-strict evaluation.
module Agda.Utils.ThunkLets (thunkLets) where

import Agda.Syntax.Treeless
import Agda.TypeChecking.Substitute
import Agda.Compiler.Treeless.Subst ( Occurs(..), occursIn )

-- | Rewrite @let x = u in b@ into @let x = λ _ → u in b[x □ / x]@.
thunkLets :: TTerm -> TTerm
thunkLets = \case
  TLet u b
    -- do not thunk if @u@ is already a value or @b@ evaluates @x@ on every path
    | isValue u' || demands 0 b' -> TLet u' b'
    -- if @b@ occurs at most once, substitute it away to avoid a spurious thunk
    | occursOnce b'              -> applySubst ([u'] ++# idS) b'
    | otherwise                  -> TLet (TLam $ raise 1 u') (force 0 b')
    where
    u' = go u
    b' = go b

  TCoerce a          -> TCoerce (go a)
  TLam b             -> TLam (go b)
  TApp a bs          -> TApp (go a) (go <$> bs)
  TCase sc ct d alts -> TCase sc ct (go d) (goAlt <$> alts)
  t                  -> t
  where
  go = thunkLets

  goAlt :: TAlt -> TAlt
  goAlt = \case
    TAGuard g b -> TAGuard (go g) (go b)
    TACon q a b -> TACon q a (go b)
    TALit l b   -> TALit l (go b)

-- | Do not not thunk let-bound values.
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

-- | Whether cbv-evaluation of the term forces variable @i@ on every path.
demands :: Int -> TTerm -> Bool
demands i = \case
  TApp (TPrim PSeq) es
    -- seq's argument does not count, since agda2lambox drops it (c.f. issue #12)
    | not (null es) -> go i (last es)
  TVar j            -> i == j
  TApp f es         -> go i f || any (go i) es
  TLet u b          -> go i u || go (i + 1) b
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
  TApp f es -> TApp (go i f) (go i <$> es)
  TLet u b  -> TLet (go i u) (go (i + 1) b)
  TLam b    -> TLam (go (i + 1) b)
  TCoerce t -> TCoerce (go i t)
  TCase sc ct d alts
    -- cannot force case scrutinee variable: introduce fresh let-binding
    | sc == i  -> TLet (forced i) $ go (i + 1) $
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


