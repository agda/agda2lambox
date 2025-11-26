{-# LANGUAGE NamedFieldPuns, FlexibleInstances, DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
module Agda2Lambox.Compile.Utils
  ( modNameToModPath
  , qnameToKName
  , qnameToName
  , qnameToIdent
  , domToName
  , dataOrRecDefMutuals
  , dataOrRecMutuals
  , toInductive
  , toConApp
  , MayBeLogical(isLogical)
  , sanitize
  ) where

import Control.Monad.State
import Control.Monad.IO.Class ( liftIO )
import Data.Char
import Data.List ( elemIndex, foldl' )
import Data.Maybe ( fromMaybe, listToMaybe, isJust )

import Unicode.Char.Identifiers
import Agda.Compiler.Backend 
import Agda.Syntax.Internal
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common.Pretty ( prettyShow, Doc )
import Agda.Syntax.Common ( usableModality, Arg(..) )
import Agda.TypeChecking.Datatypes ( getConstructors, getConstructorData )
import Agda.TypeChecking.Level ( isLevelType )
import Agda.TypeChecking.Monad.SizedTypes ( isSizeType )
import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.TypeChecking.Substitute (TelV(TelV))
import Agda.TypeChecking.Telescope (telView)
import Agda.Utils.Monad (orM)

import Agda2Lambox.Compile.Monad
import LambdaBox qualified as LBox

-- | Convert and Agda module name to its "equivalent" λ□ module path.
modNameToModPath :: ModuleName -> LBox.ModPath
modNameToModPath =
  LBox.MPFile . map (sanitize . prettyShow) . mnameToList

-- | Convert and Agda definition name to a λ□ kernel name.
qnameToKName :: QName -> LBox.KerName
qnameToKName qn =
  LBox.KerName
    (modNameToModPath $ qnameModule qn)
    (qnameToIdent qn)

domToName :: Dom' a b -> LBox.Name
domToName = maybe LBox.Anon (LBox.Named . sanitize . prettyShow) . domName

qnameToIdent :: QName -> LBox.Ident
qnameToIdent = sanitize . prettyShow . qnameName

qnameToName :: QName -> LBox.Name
qnameToName = LBox.Named . qnameToIdent

dataOrRecDefMutuals :: Definition -> TCM [QName]
dataOrRecDefMutuals d = do
  case theDef d of
    Datatype{dataMutual} -> pure $ fromMaybe [] dataMutual
    Record  {recMutual}  -> pure $ fromMaybe [] recMutual
    _                    -> internalError "Not a datatype or record"

dataOrRecMutuals :: QName -> TCM [QName]
dataOrRecMutuals q = dataOrRecDefMutuals =<< getConstInfo q

-- | Fetch the λ□ inductive associated with a @QName@.
toInductive :: QName -> TCM LBox.Inductive
toInductive q = do
  names <- dataOrRecMutuals q
  let repr = fromMaybe q $ listToMaybe names
  let idx  = fromMaybe 0 $ elemIndex q names
  pure $ LBox.Inductive (qnameToKName repr) idx


-- | Compile a constructor application to λ□.
toConApp :: QName -> [LBox.Term t] -> CompileM (LBox.Term t)
toConApp qn es = do
  dt   <- getConstructorData qn
  ctrs <- liftTCM $ getConstructors dt
  ind  <- liftTCM $ toInductive dt
  let idx = fromMaybe 0 $ qn `elemIndex` ctrs

  -- if the no blocks option is enabled
  -- no argument is given to LConstruct
  -- and we instead use regular application
  nb <- asks noBlocks
  if nb then pure $ foldl' LBox.LApp (LBox.LConstruct ind idx []) es
        else pure $ LBox.LConstruct ind idx es


-- | Class for things that may be considered logical, and thus erased.
-- See https://arxiv.org/pdf/2108.02995 for the precise definition.
--
class MayBeLogical a where
  isLogical :: a -> TCM Bool

-- * Logical types
--
-- Note that we may also want to consider logical products 
-- into logical types?, Say "proof builders", or "level builders", etc.

-- | Logical types.
--
-- A type is considered logical when it is a proposition
-- (its inhabitants are proofs) or when it is an arity in Prop.
--
-- @Size@ and @Level@ are also considered logical.
instance MayBeLogical Type where
  isLogical typ = orM
    [ pure $ isLogicalSort $ getSort typ
    , isLevelType typ
    , isJust <$> isSizeType typ
    , do TelV tel typ <- telView typ
         case unEl typ of
           Sort s -> pure $ isLogicalSort s
           _      -> pure False
    ]
    where
      isLogicalSort :: Sort -> Bool
      isLogicalSort = \case
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

-- | Sanitize an agda name to something without unicode.
-- Must be injective.
-- We may require a smarter transformation later on for other targets.
sanitize :: String -> String
sanitize xs | xs `elem` ["true", "false"] = "r#" ++ xs
sanitize [] = []
sanitize (x:xs) = toXIDStart x <> concatMap toXIDContinue xs
  where
    toXIDStart, toXIDContinue :: Char -> String
    toXIDStart c
      | isXIDStart c || c == '_' = [c]
      | isXIDContinue c          = "_" <> [c]
      | otherwise                = toXIDContinue c

    toXIDContinue c
      | isXIDContinue c = [c]
      | otherwise       = "y" <> show (ord c) <> "y"
{-
sanitize q = "agda" ++ concatMap encode q
  where
  encode '$' = "$$"
  encode c
    | isAscii c -- more agressive sanitization
    , isLower c
    || isUpper c
    || c == '_'
    || generalCategory c == DecimalNumber = [c]
    | otherwise = "$" ++ show (fromEnum c) ++ "$"

-- | Sanitize an agda name to something Rust-compliant.
-- Must be injective.
sanitize :: String -> String
sanitize [] = []
sanitize (c:cs) = toXIDStart c <> concatMap toXIDContinue cs
  where
    toXIDStart, toXIDContinue :: Char -> String
    toXIDStart c
      | isXIDStart c || c == '_' = [c]
      | isXIDContinue c          = "_" <> [c]
      | otherwise                = toXIDContinue c

    toXIDContinue c
      | isXIDContinue c = [c]
      | otherwise        = "Ֆ" <> show (ord c) <> "Ֆ"
      -}
