{-# LANGUAGE FlexibleInstances #-}
module Agda2Lambox.Compile.Utils where

import Data.List ( elemIndex, mapAccumL )
import Data.Maybe ( fromMaybe, listToMaybe, isJust )

import Agda.Compiler.Backend
import Agda.Compiler.Common qualified as CC
import Agda.Compiler.Treeless.Erase ( isErasable )
import Agda.Syntax.Internal
import Agda.Syntax.TopLevelModuleName ( moduleNameParts )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( usableModality, Arg(..), NameId(..) )
import Agda.TypeChecking.Datatypes ( getConstructors, getConstructorData )
import Agda.TypeChecking.Level ( isLevelType )
import Agda.TypeChecking.Reduce ( reduce )
import Agda.TypeChecking.Substitute ( TelV(TelV) )
import Agda.TypeChecking.Telescope ( telView, telViewUpTo, telViewPath, flattenTel )
import Agda.Utils.Monad ( orM, andM, filterM, forM )

import Agda.Utils ( getInductiveParams, isArity, dataOrRecMutuals )
import Agda2Lambox.Compile.Monad
import LambdaBox qualified as LBox

-- | Convert an Agda module name to its λ□ module path:
--   the file module as an 'MPFile', and each module nested in it as an 'MPDot'.
modNameToModPath :: ReadTCState m => ModuleName -> m LBox.ModPath
modNameToModPath m = do
  top <- CC.topLevelModuleName m
  let (file, nested) = splitAt (length $ moduleNameParts top) (map showPart $ mnameToList m)
  pure $ foldl LBox.MPDot (LBox.MPFile $ reverse file) nested
  where
    -- anonymous modules are suffixed with their name id
    showPart :: Name -> String
    showPart x
      | isNoName x, NameId i _ <- nameId x = prettyShow x <> show i
      | otherwise                          = prettyShow x

-- | Convert and Agda definition name to a λ□ kernel name.
qnameToKName :: ReadTCState m => QName -> m LBox.KerName
qnameToKName qn = do
  mp <- modNameToModPath (qnameModule qn)
  pure $ LBox.KerName mp (qnameToIdent qn)

domToName :: Dom' a b -> LBox.Name
domToName = maybe LBox.Anon (LBox.Named . prettyShow) . domName

-- NB: use of nameCanonical to handle renamed imports (c.f. Agda issue #4735)
qnameToIdent :: QName -> LBox.Ident
qnameToIdent = prettyShow . nameCanonical . qnameName

-- | Fetch the λ□ inductive associated with a @QName@.
toInductive :: QName -> TCM LBox.Inductive
toInductive q = do
  names <- dataOrRecMutuals q
  let repr = fromMaybe q $ listToMaybe names
  let idx  = fromMaybe 0 $ elemIndex q names
  kn <- qnameToKName repr
  pure $ LBox.Inductive kn idx

-- | The parameters of a datatype/record that are λ□ parameters.
inductivePars :: Definition -> TCM [Dom Type]
inductivePars Defn{theDef, defType} = do
  TelV params _ <- telViewUpTo (getInductiveParams theDef) defType
  -- only consider type parameters (non-logical arities) which inform typing
  let isTypeParam ty = andM [ isArity ty, not <$> isLogical ty ]
  addContext params $ filterM (isTypeParam . unDom) (flattenTel params)

-- | Number of parameters of the λ□ mutual block a datatype/record belongs to.
toInductivePars :: QName -> TCM Int
toInductivePars q = do
  defs <- mapM getConstInfo . (q :) =<< dataOrRecMutuals q
  pars <- forM (filter (defIsDataOrRecord . theDef) defs) $ \d ->
    length <$> inductivePars d
  -- keep the common prefix of all members of a mutual block
  return $ minimum pars

-- | Compile a constructor application to λ□.
toConApp :: QName -> [LBox.Term] -> CompileM LBox.Term
toConApp qn es = do
  dt   <- getConstructorData qn
  ctrs <- liftTCM $ getConstructors dt
  ind  <- liftTCM $ toInductive dt
  pars <- liftTCM $ toInductivePars dt
  let es' = replicate pars LBox.LBox ++ es -- replace type parameters with □
  let idx = fromMaybe 0 $ qn `elemIndex` ctrs
  pure $ foldl' LBox.LApp (LBox.LConstruct ind idx []) es'

-- | Apply the main program's (erased) module parameters.
mainProgram :: QName -> TCM LBox.Term
mainProgram q = do
  kn <- qnameToKName q
  es <- erasableParams =<< lookupSection (qnameModule q)
  let r    = length $ filter not es
      arg  = \j b -> if b then (j, LBox.LBox) else (j - 1, LBox.LRel j)
      body = foldl' LBox.LApp (LBox.LConst kn)
           $ snd
           $ mapAccumL arg (r - 1) es
  pure $ iterate (LBox.LLambda LBox.Anon) body !! r
  where
  erasableParams :: Telescope -> TCM [Bool]
  erasableParams = \case
    EmptyTel      -> pure []
    ExtendTel d t -> (:) <$> orM [isLogical d, isArity (unDom d)]
                         <*> underAbstraction d t erasableParams

-- | Class for things that may be considered logical, and thus erased.
-- See https://arxiv.org/pdf/2108.02995 for the precise definition.
--
class MayBeLogical a where
  isLogical :: a -> TCM Bool

-- * Logical types

-- | Logical types.
--
-- A type is considered logical when it is a proposition
-- (its inhabitants are proofs) or when it is an arity in Prop.
--
-- Products into @Size@ and @Level@ are also considered logical.
instance MayBeLogical Type where
  isLogical typ = do
    TelV tel cod <- telView typ
    addContext tel $ orM
      [ isLogicalSort $ getSort cod
      , isLevelType cod
      , isJust <$> isSizeType cod
      , case unEl cod of
          Sort s -> isLogicalSort s
          _      -> pure False
      ]
    where
      -- @LevelUniv@ is matched before reduction, as it reduces to @Set@ without --level-universe.
      isLogicalSort :: Sort -> TCM Bool
      isLogicalSort = \case
        LevelUniv{} -> pure True
        s -> reduce s <&> \case
          Prop{}      -> True -- Prop
          Inf UProp _ -> True -- Propw
          SizeUniv{}  -> True -- SizeUniv
          LevelUniv{} -> True -- LevelUniv
          _           -> False

-- | Additionally, we consider erased domains logical.
instance MayBeLogical a => MayBeLogical (Dom a) where
  isLogical dom | not (usableModality dom) = pure True
  isLogical dom = isLogical $ unDom dom

instance MayBeLogical a => MayBeLogical (Arg a) where
  isLogical arg | not (usableModality arg) = pure True
  isLogical arg = isLogical $ unArg arg

-- * Erased types

-- | How Agda's erasure treats the values of a type.
--   (c.f. Agda.Compiler.Treeless.Erase.getTypeInfo)
data TypeInfo = Empty | Erasable | NotErasable
  deriving Eq

erasable :: TypeInfo -> Bool
erasable = (/= NotErasable)

typeInfo :: Type -> TCM TypeInfo
typeInfo = typeInfo' []

typeInfo' :: [QName] -> Type -> TCM TypeInfo
typeInfo' seen ty = do
  TelV tel cod <- telViewPath ty
  is <- domains tel
  et <- addContext tel case unEl cod of
    Def d _ -> headInfo' seen d
    Sort{}  -> pure Erasable
    _       -> pure NotErasable
  pure if | Empty `elem` is -> Erasable
          | null is         -> et
          | et == Empty     -> Erasable
          | otherwise       -> et
  where
  domains = \case
    EmptyTel        -> pure []
    ExtendTel a tel -> (:) <$> typeInfo' seen (unDom a) <*> underAbstraction a tel domains

-- | How Agda's erasure treats the values of a type headed by a definition.
headInfo :: QName -> TCM TypeInfo
headInfo = headInfo' []

headInfo' :: [QName] -> QName -> TCM TypeInfo
headInfo' seen q
  | q `elem` seen = pure Erasable
  | otherwise = do
    sizes <- mapM getBuiltinName' [builtinSize, builtinSizeLt]
    if Just q `elem` sizes then pure Erasable else theDef <$> getConstInfo q >>= \case
      Datatype{dataCons = []}    -> pure Empty
      Datatype{dataCons = c : _} -> fromErasable <$> isErasable c
      Record{recConHead}         -> fromErasable <$> isErasable (conName recConHead)
      Function{funClauses}       -> foldr plus Empty <$> mapM clauseInfo funClauses
      _                          -> pure NotErasable
  where
  fromErasable b = if b then Erasable else NotErasable

  clauseInfo cl = case clauseBody cl of
    Nothing -> pure Empty
    Just b  -> addContext (clauseTel cl) $ typeInfo' (q : seen) (El __DUMMY_SORT__ b)

  plus Empty       r            = r
  plus  r           Empty       = r
  plus  Erasable    r           = r
  plus  r           Erasable    = r
  plus  NotErasable NotErasable = NotErasable

-- | Whether Agda erases the values of a function type due to an empty domain.
isAbsurdFunType :: Dom Type -> TCM Bool
isAbsurdFunType dom
  | not (usableModality dom) = pure False
  | otherwise = do
    TelV tel cod <- telViewPath $ unDom dom
    if null tel then pure False else andM
      [ (== Erasable) <$> typeInfo (unDom dom)
      , addContext tel $ (== NotErasable) <$> typeInfo cod
      ]


