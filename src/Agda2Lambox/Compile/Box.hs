-- | Typed λ□ gives type □ to whatever Agda's erasure removes from some terms,
--   but Agda keeps it in others, since it decides part of its erasure per definition.
module Agda2Lambox.Compile.Box ( insertBoxes ) where

import Data.List ( findIndices )

import Agda.Compiler.Backend ( getConstInfo, getErasedConArgs, underAbstraction )
import Agda.Compiler.Treeless.Erase ( computeErasedConstructorArgs )
import Agda.Syntax.Common ( Arg(..) )
import Agda.Syntax.Internal ( Tele(..) )
import Agda.Syntax.Treeless
import Agda.TypeChecking.Datatypes ( getConstructorData )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Signature ( getCompiledArgUse )
import Agda.TypeChecking.Substitute ( raise, TelV(..) )
import Agda.TypeChecking.Telescope ( telViewPath )
import Agda.Compiler.Treeless.Subst ()
import Agda.Utils.Monad ( ifM )
import Agda.Utils.List ( lastMaybe )

import Agda2Lambox.Compile.Utils ( isLogical, isAbsurdFunType )

-- | Insert □ for:
--   * applications of logical definitions that Agda does not erase, e.g. projections into @Level@;
--   * erased constructor fields, e.g. forced ones bound by 'etaExpandConstructors';
--   * unused arguments, which Agda keeps in calls compiled before the callee's body;
--
-- Insert an absurd lambda instead of □ for:
--   * arguments/fields of a function type with empty domain
--
-- Partial applications are η-expanded up to their last unused argument,
-- since Peregrine dearges a constant only where it is applied to all of them.
insertBoxes :: TTerm -> TCM TTerm
insertBoxes t = case tAppView t of

  (TDef f, es) -> ifM (isLogicalDef f) (pure TErased) do
    unused <- maybe [] (findIndices (== ArgUnused)) <$> getCompiledArgUse f
    absurd <- absurdArgs f
    etaBoxes unused (TDef f) . zipWith3 arg (repeat False) absurd <$> mapM insertBoxes es

  (TCon c, es) -> do
    erased <- erasedConArgs c
    absurd <- absurdArgs c
    mkTApp (TCon c) . zipWith3 arg (erased ++ repeat False) absurd <$> mapM insertBoxes es

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

  arg = \case
    _    True _ -> TLam tUnreachable
    True _    _ -> TErased
    _    _    e -> e

  absurdArgs q = do
    Defn{defType, theDef} <- getConstInfo q
    TelV tel _ <- telViewPath defType
    let pars = case theDef of
          Constructor{conPars}                                  -> conPars
          Function{funProjection = Right Projection{projIndex}} -> projIndex - 1
          _                                                     -> 0
    (++ repeat False) . drop pars <$> go tel
    where
    go = \case
      EmptyTel        -> pure []
      ExtendTel a tel -> (:) <$> isAbsurdFunType a <*> underAbstraction a tel go

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
