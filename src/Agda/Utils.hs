{-# LANGUAGE FlexibleContexts, OverloadedStrings, BangPatterns, NamedFieldPuns #-}

-- | Agda utilities.
module Agda.Utils where

import Control.Applicative ( liftA2 )
import Data.Bifunctor ( second )
import Data.Functor ( (<&>) )
import Data.Maybe ( isJust, isNothing, catMaybes, fromMaybe )
import Data.Text ( Text )

import Agda.Compiler.Backend ( getUniqueCompilerPragma, PureTCM, HasConstInfo (getConstInfo) )
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common ( defaultArg )
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base hiding ( conArity )
import Agda.TypeChecking.Datatypes ( getConstructorInfo, ConstructorInfo(..) )
import Agda.TypeChecking.Substitute ( raise, TelV (theCore) )
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Treeless

import Agda.Compiler.ToTreeless                 qualified as TT
import Agda.Compiler.Treeless.Builtin           qualified as TT
import Agda.Compiler.Treeless.Erase             qualified as TT
import Agda.Compiler.Treeless.Simplify          qualified as TT
import Agda.Compiler.Treeless.Identity          qualified as TT
import Agda.Compiler.Treeless.Uncase            qualified as TT
import Agda.Compiler.Treeless.AsPatterns        qualified as TT
import Agda.TypeChecking.Telescope (telView, telViewUpTo)
import Agda.TypeChecking.Substitute (TelV(theTel))

import Agda.Utils.EliminateDefaults qualified as TT
import Agda.Utils.Treeless qualified as CustomTT
import Agda.TypeChecking.Reduce (reduceDefCopy)

-- * Miscellaneous

-- | Pragma string for the agda2lambox backend.
agda2lamboxP :: Text
agda2lamboxP = "AGDA2LAMBOX"

pp :: Pretty a => a -> String
pp = prettyShow

hasPragma :: QName -> TCM Bool
hasPragma qn = isJust <$> getUniqueCompilerPragma agda2lamboxP qn

-- | Retrieve the additional info to a COMPILE pragma, if any
getPragmaInfo :: QName -> TCM (Maybe String)
getPragmaInfo qn =
  getUniqueCompilerPragma agda2lamboxP qn <&> \case
    Nothing                      -> Nothing
    Just (CompilerPragma _ info) -> Just info

-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False

-- | Check whether a definition is a datatype/record.
isDataDef, isRecDef, isFunDef, isDataOrRecDef :: Defn -> Bool
isDataDef = \case Datatype{} -> True; _ -> False
isRecDef  = \case Record{}   -> True; _ -> False
isFunDef  = \case Function{} -> True; _ -> False
isDataOrRecDef = liftA2 (||) isDataDef isRecDef

dataOrRecDefMutuals :: Definition -> TCM [QName]
dataOrRecDefMutuals d = do
  case theDef d of
    Datatype{dataMutual} -> pure $ fromMaybe [] dataMutual
    Record  {recMutual}  -> pure $ fromMaybe [] recMutual
    _                    -> internalError "Not a datatype or record"

dataOrRecMutuals :: QName -> TCM [QName]
dataOrRecMutuals q = dataOrRecDefMutuals =<< getConstInfo q

getInductiveParams :: Defn -> Int
getInductiveParams Datatype{dataPars} = dataPars
getInductiveParams Record{recPars}    = recPars

-- TODO: consider erased things to be logical
--       so maybe take a Dom Type instead

-- | Whether a type is an arity.
isArity :: Type -> TCM Bool
isArity typ = do
  tel <- telView typ
  pure $ isJust $ isSort $ unEl $ theCore $ tel

-- | Whether a type is a Π type.
isPiTy :: Type -> TCM Bool
isPiTy ty = not . null . theTel <$> telViewUpTo 1 ty

-- | Eta-expand treeless term only if it's not already a lambda abstraction.
etaExpand :: TTerm -> TTerm
etaExpand = \case
  t@TLam{} -> t
  t        -> TLam $ TApp (raise 1 t) [TVar 0]

-- ** toTreeless custom pipeline

-- | Convert compiled clauses to treeless syntax, and return it.
treeless :: QName -> TCM (Maybe TTerm)
treeless = CustomTT.toTreeless EagerEvaluation
  
-- * projections

isRecordProjection :: Defn -> Maybe (QName, QName)
isRecordProjection d
  | Function{..} <- d
  , Right Projection{..} <- funProjection
  , Just recName <- projProper
  = Just (recName, projOrig)
  | otherwise
  = Nothing

-- | Try to unfold a definition if introduced by module application.
maybeUnfoldCopy
  :: PureTCM m
  => QName -- ^ Name of the definition.
  -> Elims
  -> (Term -> m a)
  -- ^ Callback if the definition is indeed a copy.
  -> (QName -> Elims -> m a)
  -- ^ Callback if the definition isn't a copy.
  -> m a
maybeUnfoldCopy f es onTerm onDef =
  reduceDefCopy f es >>= \case
    NoReduction ()   -> onDef f es
    YesReduction _ t -> onTerm t

-- | Remove all the names defined in where clauses from the list of names.
-- Assumes that the order is the one given by Agda, that is:
-- definitions in (possibly anonymous) modules coming from where clauses appear 
-- right after their parent function definition.
filterOutWhere :: [QName] -> TCM [QName]
filterOutWhere [] = pure []
filterOutWhere (q:qs) = do
  ws <- getWheres q
  let qs' = dropWhile (isChild ws) qs
  (q:) <$> filterOutWhere qs'

  where
  isChild :: [ModuleName] -> QName -> Bool
  isChild ws r = any (r `isChildOfMod`) ws

  isChildOfMod :: QName -> ModuleName -> Bool
  isChildOfMod q mod = qnameModule q `isLeChildModuleOf` mod

  getWheres :: QName -> TCM [ModuleName]
  getWheres q = do
    def <- getConstInfo q
    pure case theDef def of
      Function{..} -> catMaybes $ map clauseWhereModule funClauses
      _            -> []

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
