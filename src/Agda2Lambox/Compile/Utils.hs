{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}
module Agda2Lambox.Compile.Utils where

import Control.Monad.State
import Control.Monad.IO.Class ( liftIO )
import Data.Functor ( (<&>) )
import Data.List ( elemIndex, foldl' )
import Data.Maybe ( fromMaybe, listToMaybe, isJust )

import Agda.Compiler.Backend
import Agda.Syntax.Internal
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common.Pretty ( prettyShow, Doc )
import Agda.Syntax.Common ( usableModality, Arg(..), NameId(..) )
import Agda.TypeChecking.Datatypes ( getConstructors, getConstructorData )
import Agda.TypeChecking.Level ( isLevelType )
import Agda.TypeChecking.Monad.SizedTypes ( isSizeType )
import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.TypeChecking.Reduce ( reduce )
import Agda.TypeChecking.Substitute ( TelV(TelV), theTel )
import Agda.TypeChecking.Telescope ( telView, telViewUpTo, flattenTel )
import Agda.Utils.Monad ( orM, andM, filterM, forM )

import Agda.Utils ( getInductiveParams, isDataOrRecDef, isArity, dataOrRecMutuals )
import Agda2Lambox.Compile.Monad
import LambdaBox qualified as LBox

-- | Convert an Agda module name to its "equivalent" λ□ module path.
modNameToModPath :: ModuleName -> LBox.ModPath
modNameToModPath = LBox.MPFile . map showPart . mnameToList
  where
    -- anonymous modules are suffixed with their name id
    showPart :: Name -> String
    showPart x
      | isNoName x, NameId i _ <- nameId x = prettyShow x <> show i
      | otherwise                          = prettyShow x

-- | Convert and Agda definition name to a λ□ kernel name.
qnameToKName :: QName -> LBox.KerName
qnameToKName qn =
  LBox.KerName
    (modNameToModPath $ qnameModule qn)
    (qnameToIdent qn)

domToName :: Dom' a b -> LBox.Name
domToName = maybe LBox.Anon (LBox.Named . prettyShow) . domName

qnameToIdent :: QName -> LBox.Ident
qnameToIdent = prettyShow . nameCanonical . qnameName
-- NB: use of nameCanonical to handle renamed imports (c.f. Agda issue #4735)

qnameToName :: QName -> LBox.Name
qnameToName = LBox.Named . qnameToIdent

-- | Fetch the λ□ inductive associated with a @QName@.
toInductive :: QName -> TCM LBox.Inductive
toInductive q = do
  names <- dataOrRecMutuals q
  let repr = fromMaybe q $ listToMaybe names
  let idx  = fromMaybe 0 $ elemIndex q names
  pure $ LBox.Inductive (qnameToKName repr) idx

-- | The parameters of a datatype/record that are λ□ parameters.
inductivePars :: Definition -> TCM [Dom Type]
inductivePars Defn{theDef, defType} = do
  let n = getInductiveParams theDef
  params <- flattenTel . theTel <$> telViewUpTo n defType
  -- only consider type parameters (non-logical arities) which inform typing
  let isTypeParam ty = andM [ isArity ty, not <$> isLogical ty ]
  addContext params $ filterM (isTypeParam . unDom) params

-- | Number of parameters of the λ□ mutual block a datatype/record belongs to.
toInductivePars :: QName -> TCM Int
toInductivePars q = do
  defs <- mapM getConstInfo . (q :) =<< dataOrRecMutuals q
  pars <- forM (filter (isDataOrRecDef . theDef) defs) $ \d ->
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
