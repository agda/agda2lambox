{-# LANGUAGE OverloadedStrings #-}

-- | Agda utilities.
module Agda.Utils where

import Data.Maybe ( isJust, fromMaybe )

import Agda.Compiler.Backend ( getUniqueCompilerPragma, HasConstInfo (getConstInfo) )
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common ( defaultArg, namedThing, unArg )
import Agda.Syntax.Internal
import Agda.Syntax.Treeless
import Agda.TypeChecking.Monad.Base hiding ( conArity )
import Agda.TypeChecking.Datatypes ( ConstructorInfo(..), getConstructorInfo )
import Agda.TypeChecking.Substitute ( raise, TelV(..) )
import Agda.TypeChecking.Telescope ( telView, telViewUpTo )

-- * Miscellaneous

-- | Whether a definition has an @AGDA2LAMBOX@ COMPILE pragma.
hasPragma :: QName -> TCM Bool
hasPragma qn = isJust <$> getUniqueCompilerPragma "AGDA2LAMBOX" qn

-- | Check whether a definition is a function.
isFunDef :: Defn -> Bool
isFunDef = \case Function{} -> True; _ -> False

dataOrRecDefMutuals :: Definition -> TCM [QName]
dataOrRecDefMutuals d = case theDef d of
  Datatype{dataMutual} -> pure $ fromMaybe [] dataMutual
  Record  {recMutual}  -> pure $ fromMaybe [] recMutual
  _                    -> internalError "Not a datatype or record"

dataOrRecMutuals :: QName -> TCM [QName]
dataOrRecMutuals q = dataOrRecDefMutuals =<< getConstInfo q

getInductiveParams :: Defn -> Int
getInductiveParams Datatype{dataPars} = dataPars
getInductiveParams Record{recPars}    = recPars

-- | Number of arguments of a constructor, excluding parameters.
getConArity :: QName -> TCM Int
getConArity q = getConstructorInfo q <&> \case
  DataCon a         -> a
  RecordCon _ _ a _ -> a

-- | Whether a type is an arity.
isArity :: Type -> TCM Bool
isArity = fmap (isJust . isSort . unEl . theCore) . telView

-- | Whether a type is a Π type.
isPiTy :: Type -> TCM Bool
isPiTy ty = not . null . theTel <$> telViewUpTo 1 ty

-- | Eta-expand treeless term only if it's not already a lambda abstraction.
etaExpand :: TTerm -> TTerm
etaExpand = \case
  t@TLam{} -> t
  t        -> TLam $ TApp (raise 1 t) [TVar 0]

-- | Apply a transformation to the immediate subterms of a treeless term.
descendM :: Applicative m => (TTerm -> m TTerm) -> TTerm -> m TTerm
descendM f = \case
  TApp a bs          -> TApp <$> f a <*> traverse f bs
  TLam b             -> TLam <$> f b
  TLet s u b         -> TLet s <$> f u <*> f b
  TCase sc ct d alts -> TCase sc ct <$> f d <*> traverse (descendAltM f) alts
  TCoerce a          -> TCoerce <$> f a
  t                  -> pure t

descendAltM :: Applicative m => (TTerm -> m TTerm) -> TAlt -> m TAlt
descendAltM f = \case
  TACon c n b -> TACon c n <$> f b
  TALit l b   -> TALit l <$> f b
  TAGuard g b -> TAGuard <$> f g <*> f b

-- * projections

-- | How many parameters Agda drops from clauses/applications of projection(-like) functions.
droppedParams :: Defn -> Int
droppedParams = \case
  Function{funProjection = Right Projection{projIndex}} -> projIndex - 1
  _                                                     -> 0

-- | Whether a type-level function is compiled to a type alias:
--   it has a single clause, whose patterns are variables.
isTypeAlias :: Defn -> Bool
isTypeAlias = \case
  Function{funClauses = [cl]} ->
    all (isVarP . namedThing . unArg) (namedClausePats cl) && isJust (clauseBody cl)
  _ -> False
  where isVarP = \case VarP{} -> True; _ -> False

-- | Unspines 1 layer of a term,
-- transforming eventual projection elims into 'Def' projection applications.
unSpine1 :: Term -> Term
unSpine1 v =
  case hasElims v of
    Just (h, es) -> fromMaybe v $ loop h [] es
    Nothing      -> v
  where
    loop :: (Elims -> Term) -> Elims -> Elims -> Maybe Term
    loop h res es =
      case es of
        []             -> Nothing
        Proj o f : es' -> Just $ fromMaybe (Def f (Apply (defaultArg v) : es')) $ loop h (Proj o f : res) es'
        e        : es' -> loop h (e : res) es'
      where v = h $ reverse res
