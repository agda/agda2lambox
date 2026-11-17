{-# LANGUAGE OverloadedStrings #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Compile.Function
  ( compileFunction
  ) where

import Control.Monad ( forM, filterM )
import Data.Graph ( stronglyConnComp, SCC(..) )
import Data.List ( elemIndex, find, sort )
import Data.Map ( Map )
import Data.Map qualified as M
import Data.Maybe ( fromMaybe, isJust )
import Data.Set ( Set )
import Data.Set qualified as S

import Agda.Syntax.Abstract.Name ( QName )
import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend ( getConstInfo, reportSDoc )
import Agda.Compiler.Treeless.Erase ( isErasable )
import Agda.Syntax.Treeless ( ArgUsage(..) )
import Agda.TypeChecking.Monad.Signature ( getCompiledArgUse )
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils.Monad ( ifM, ifNotM )
import Agda.Utils.Treeless ( treeless )

import Agda.Utils ( droppedParams, isPiTy, etaExpand, isFunDef )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Box ( insertBoxes )
import Agda2Lambox.Compile.Term ( compileTerm )
import Agda2Lambox.Compile.Type ( compileTopLevelType )

import LambdaBox qualified as LBox
import LambdaBox.Env


-- | Convert a function body to a λ□ term.
compileFunctionBody :: Target t -> [QName] -> Definition -> CompileM LBox.Term
compileFunctionBody t ms Defn{defName, defType} = do
  Just body <- liftTCM $ treeless t defName
  reportSDoc "agda2lambox.compile.function" 10 $
    "treeless body:" <+> pretty body
  -- fixpoint bodies must always be lambdas, so η-expand recursive functions of
  -- function type, e.g. recursive pattern-lambdas that are not inlined by Agda
  isPi <- liftTCM $ isPiTy defType
  let body' = if not (null ms) && isPi then etaExpand body else body
  compileTerm ms =<< case t of
    ToTyped   -> liftTCM $ insertBoxes body'
    ToUntyped -> pure body'

-- | Whether to compile a function definition to λ□:
--    ∙ it is non-erased
--    ∙ the treeless translation does not always inline it
shouldCompileFunction :: Target t -> Definition -> TCM Bool
shouldCompileFunction t def@Defn{defName}
  | hasQuantityω def = isJust <$> treeless t defName
  | otherwise        = pure False

type MutualGraph = Map QName (Set QName)

-- | Detect the mutually-recursive group of a function via treeless traversal.
--
-- NB: see the discussion on Agda issue #8111 on why @funMutuals@ is not enough,
-- i.e. the positivity checker misses some calls on agruments it deems unused.
recursiveGroup :: Target t -> Definition -> TCM [QName]
recursiveGroup t Defn{defName, defMutual = block} =
  extractGroup <$> explore M.empty [defName]
  where
  explore :: MutualGraph -> [QName] -> TCM MutualGraph
  explore gr [] = pure gr
  explore gr (q:qs)
    | q `M.member` gr
    = explore gr qs
    | otherwise
    = calls q >>= \cs -> explore (M.insert q cs gr) (S.toList cs ++ qs)

  calls :: QName -> TCM (Set QName)
  calls q = treeless t q >>= \case
    Nothing -> pure S.empty
    Just t  -> S.fromList <$> filterM inBlock (S.toList $ namesIn t)

  inBlock :: QName -> TCM Bool
  inBlock q = getConstInfo q <&> \def -> defMutual def == block && isFunDef (theDef def)

  extractGroup :: MutualGraph -> [QName]
  extractGroup g =
    let sccs = stronglyConnComp [ (q, q, S.toList qs) | (q, qs) <- M.toList g ]
    in fromMaybe [] $ find (defName `elem`) [ sort qs | CyclicSCC qs <- sccs ]

-- | Convert a function definition to a λ□ declaration.
compileFunction :: Target t -> Definition -> CompileM (Maybe (LBox.GlobalDecl t))
compileFunction t defn@Defn{defType} =
  ifNotM (liftTCM $ shouldCompileFunction t defn) (pure Nothing) do
  mnames <- liftTCM $ recursiveGroup t defn
  mdefs  <- liftTCM $ mapM getConstInfo mnames
  reportSDoc "agda2lambox.compile.function" 5 $
    "Recursive group:" <+> prettyTCM mnames
  -- (conditionally) compile type of function, which is □ when Agda erases its body,
  -- with □ for the arguments it erases from applications (c.f. 'eraseTerms')
  typ <- whenTyped t $ ifM (liftTCM $ isErasable $ defName defn) (pure ([], LBox.TBox)) do
    unused <- map (== ArgUnused) . fromMaybe [] <$> liftTCM (getCompiledArgUse $ defName defn)
    -- a projection(-like) function takes no parameters, but they may occur in its type
    let pars = droppedParams $ theDef defn
    fmap (LBox.dropArrows pars) <$> compileTopLevelType (replicate pars False ++ unused) defType

  let builder = Just . ConstantDecl . ConstantBody typ . Just

  -- if the function is not recursive, just compile the body
  if null mdefs then builder <$> compileFunctionBody t [] defn

  -- otherwise, take fixpoint
  else do
    let k = fromMaybe 0 $ elemIndex (defName defn) mnames

    builder . flip LBox.LFix k <$>
      forM mdefs \def@Defn{defName} -> do
        body <- compileFunctionBody t mnames def >>= \case
          l@LBox.LLambda{} -> pure l
          LBox.LBox        -> pure $ LBox.LLambda LBox.Anon LBox.LBox
          _                -> genericError "Fixpoint body must be Lambda."
        pure $ LBox.Def (LBox.Named $ qnameToIdent defName) body 0
