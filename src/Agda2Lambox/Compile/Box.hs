-- | Typed λ□ gives type □ to whatever Agda's erasure removes from some terms,
--   but Agda keeps it in others, since it decides part of its erasure per definition.
module Agda2Lambox.Compile.Box ( insertBoxes ) where

import Data.List ( findIndices )

import Agda.Compiler.Backend ( getConstInfo, getErasedConArgs )
import Agda.Compiler.Treeless.Erase ( computeErasedConstructorArgs )
import Agda.Syntax.Common ( Arg(..) )
import Agda.Syntax.Treeless
import Agda.TypeChecking.Datatypes ( getConstructorData )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Signature ( getCompiledArgUse )
import Agda.TypeChecking.Substitute ( raise )
import Agda.Compiler.Treeless.Subst ()
import Agda.Utils.Monad ( ifM )
import Agda.Utils.List ( lastMaybe )

import Agda2Lambox.Compile.Utils ( isLogical )

-- | Insert □ for:
--   * applications of logical definitions that Agda does not erase, e.g. projections into @Level@;
--   * erased constructor fields, e.g. forced ones bound by 'etaExpandConstructors';
--   * unused arguments, which Agda keeps in calls compiled before the callee's body;
--   partial applications are η-expanded up to their last unused argument,
--   since Peregrine dearges a constant only where it is applied to all of them.
insertBoxes :: TTerm -> TCM TTerm
insertBoxes t = case tAppView t of

  (TDef f, es) -> ifM (isLogicalDef f) (pure TErased) do
    unused <- maybe [] (findIndices (== ArgUnused)) <$> getCompiledArgUse f
    etaBoxes unused (TDef f) <$> mapM insertBoxes es

  (TCon c, es) -> do
    erased <- erasedConArgs c
    mkTApp (TCon c) . zipWith box (erased ++ repeat False) <$> mapM insertBoxes es

  _ -> case t of
    TApp f es       -> TApp <$> insertBoxes f <*> mapM insertBoxes es
    TLam b          -> TLam <$> insertBoxes b
    TLet s u b      -> TLet s <$> insertBoxes u <*> insertBoxes b
    TCase x ct d bs -> TCase x ct <$> insertBoxes d <*> mapM insertBoxesAlt bs
    TCoerce u       -> TCoerce <$> insertBoxes u
    _               -> pure t
  where
  insertBoxesAlt = \case
    TACon c n b -> TACon c n <$> insertBoxes b
    TALit l b   -> TALit l   <$> insertBoxes b
    TAGuard g b -> TAGuard   <$> insertBoxes g <*> insertBoxes b

  box True _ = TErased
  box _    e = e

  isLogicalDef f = do
    Defn{defArgInfo, defType} <- getConstInfo f
    isLogical $ Arg defArgInfo defType

  erasedConArgs c = theDef <$> getConstInfo c >>= \case
    Constructor{conErased = Nothing} -> do
      computeErasedConstructorArgs =<< getConstructorData c
      getErasedConArgs c
    _ -> getErasedConArgs c

-- | Apply a head to arguments, with □ for the unused ones (given by position),
-- abstracting over the missing arguments up to the last unused one.
-- Non-atomic arguments are let-bound, so that they are still evaluated once.
etaBoxes :: [Int] -> TTerm -> [TTerm] -> TTerm
etaBoxes unused h es
  | missing <= 0 = mkTApp h $ boxes es
  | otherwise    = bind (boxes es) []
  where
  missing = maybe 0 (+ 1) (lastMaybe unused) - length es

  boxes = zipWith (\i e -> if i `elem` unused then TErased else e) [0..]

  -- @vs@ are the arguments bound so far, in the current scope
  bind [] vs = iterate TLam body !! missing
    where body = mkTApp (raise missing h) $ boxes $
                   raise missing (reverse vs) ++ map TVar (reverse [0 .. missing - 1])
  bind (e : es) vs
    | atomic e  = bind es (e : vs)
    | otherwise = TLet Strict e $ bind (raise 1 es) (TVar 0 : raise 1 vs)

  atomic = \case
    TVar{}   -> True
    TDef{}   -> True
    TCon{}   -> True
    TPrim{}  -> True
    TLit{}   -> True
    TErased  -> True
    TUnit    -> True
    TSort    -> True
    _        -> False
