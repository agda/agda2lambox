{-# LANGUAGE NamedFieldPuns, DataKinds, OverloadedStrings #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Compile.Function
  ( compileFunction
  ) where

import Control.Monad ( forM, when, filterM, unless )
import Control.Monad.IO.Class ( liftIO )
import Data.Graph ( stronglyConnComp, SCC(..) )
import Data.List ( elemIndex, find, sort )
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Maybe ( fromMaybe )
import Data.Set ( Set )
import Data.Set qualified as S

import Agda.Syntax.Abstract.Name ( QName, qnameModule )
import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend ( getConstInfo, reportSDoc )
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils.Monad (guardWithError, whenM, ifNotM)
import Agda.Utils.Treeless ( alwaysInline, Erasure )

import Agda.Utils ( treeless, pp, isRecordProjection, isPiTy, etaExpand, isFunction )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Term ( compileTerm )
import Agda2Lambox.Compile.Type ( compileTopLevelType )

import LambdaBox qualified as LBox
import LambdaBox.Env
import Agda.TypeChecking.Telescope ( telViewUpTo )
import Agda.TypeChecking.Substitute ( TelV(..), telePi )


-- | Convert a function body to a λ□ term.
compileFunctionBody :: Erasure -> [QName] -> Definition -> CompileM LBox.Term
compileFunctionBody er ms Defn{defName, defType} = do
  Just t <- liftTCM $ treeless er defName
  reportSDoc "agda2lambox.compile.function" 10 $
    "treeless body:" <+> pretty t
  -- fixpoint bodies must always be lambdas, so η-expand recursive functions of
  -- function type, e.g. recursive pattern-lambdas that are not inlined by Agda
  isPi <- liftTCM $ isPiTy defType
  compileTerm ms $ if not (null ms) && isPi then etaExpand t else t

-- | Whether to compile a function definition to λ□:
--    ∙ it is non-erased
--    ∙ the treeless translation does not always inline it
shouldCompileFunction :: Definition -> TCM Bool
shouldCompileFunction def@Defn{defName}
  -- only non-erased
  | hasQuantityω def = not <$> alwaysInline defName
  | otherwise        = pure False

type MutualGraph = Map QName (Set QName)

-- | Detect the mutually-recursive group of a function via treeless traversal.
--
-- NB: see the discussion on Agda issue #8111 on why @funMutuals@ is not enough,
-- i.e. the positivity checker misses some calls on agruments it deems unused.
recursiveGroup :: Erasure -> Definition -> TCM [QName]
recursiveGroup er Defn{defName, defMutual = block} =
  explore M.empty [defName] >>= pure . extractGroup
  where
  explore :: MutualGraph -> [QName] -> TCM MutualGraph
  explore gr [] = pure gr
  explore gr (q:qs)
    | q `M.member` gr
    = explore gr qs
    | otherwise
    = calls q >>= \cs -> explore (M.insert q cs gr) (S.toList cs ++ qs)

  calls :: QName -> TCM (Set QName)
  calls q = treeless er q >>= \case
    Nothing -> pure S.empty
    Just t  -> S.fromList <$> filterM inBlock (S.toList $ namesIn t)

  inBlock :: QName -> TCM Bool
  inBlock q = do
    def <- getConstInfo q
    pure $ defMutual def == block && isFunction def

  extractGroup :: MutualGraph -> [QName]
  extractGroup g =
    let sccs = stronglyConnComp [ (q, q, S.toList qs) | (q, qs) <- M.toList g ]
    in fromMaybe [] $ find (defName `elem`) [ sort qs | CyclicSCC qs <- sccs ]

-- | Convert a function definition to a λ□ declaration.
compileFunction :: Target t -> Definition -> CompileM (Maybe (LBox.GlobalDecl t))
compileFunction (t :: Target t) defn@Defn{defType} =
  ifNotM (liftTCM $ shouldCompileFunction defn) (pure Nothing) do
  let er = targetErasure t
  mnames <- liftTCM $ recursiveGroup er defn
  mdefs  <- liftTCM $ mapM getConstInfo mnames
  reportSDoc "agda2lambox.compile.function" 5 $
    "Recursive group:" <+> prettyTCM mnames
  -- (conditionally) compile type of function
  typ <- whenTyped t $ case isRecordProjection (theDef defn) of
    Nothing -> compileTopLevelType defType

    -- if it is a (real) projection, drop the parameters from the type
    Just (recName, _) -> do
      Defn{defType = recType, theDef = Record{recPars}} <- liftTCM $ getConstInfo recName
      TelV pars _      <- telViewUpTo recPars recType
      TelV _ projType  <- telViewUpTo recPars defType
      fmap (LBox.dropArrows recPars) <$> compileTopLevelType (telePi pars projType)

  let builder :: LBox.Term -> Maybe (LBox.GlobalDecl t)
      builder = Just . ConstantDecl . ConstantBody typ . Just

  -- if the function is not recursive, just compile the body
  if null mdefs then builder <$> compileFunctionBody er [] defn

  -- otherwise, take fixpoint
  else do
    let k = fromMaybe 0 $ elemIndex (defName defn) mnames

    builder . flip LBox.LFix k <$>
      forM mdefs \def@Defn{defName} -> do
        body <- compileFunctionBody er mnames def >>= \case
          l@LBox.LLambda{} -> pure l
          LBox.LBox        -> pure $ LBox.LLambda LBox.Anon LBox.LBox
          _                -> genericError "Fixpoint body must be Lambda."
        return LBox.Def
          { dName = qnameToName defName
          , dBody = body
          , dArgs = 0
          }
