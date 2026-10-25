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
import Data.Maybe ( isNothing, fromMaybe )
import Data.Set ( Set )
import Data.Set qualified as S

import Data.Foldable (toList)
import Agda.Syntax.Abstract.Name ( QName, qnameModule )
import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.TypeChecking.Pretty
import Agda.Compiler.Backend ( getConstInfo, funInline, reportSDoc )
import Agda.Syntax.Internal (domName)
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils.Monad (guardWithError, whenM)
import Agda.Utils.Lens ( (^.) )

import Agda.Utils ( treeless, pp, isRecordProjection, isArity )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Term ( compileTerm )
import Agda2Lambox.Compile.Type ( compileTopLevelType, compileType )

import LambdaBox qualified as LBox
import LambdaBox.Env
import Agda.TypeChecking.Telescope (telViewUpTo)
import Agda.TypeChecking.Substitute (TelV(theCore, theTel))


-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False


-- | Convert a function body to a Lambdabox term.
compileFunctionBody :: [QName] -> Definition -> CompileM LBox.Term
compileFunctionBody ms Defn{defName, theDef} = do
  Just t <- liftTCM $ treeless defName

  reportSDoc "agda2lambox.compile.function" 10 $
    "treeless body:" <+> pretty t

  compileTerm ms t


-- | Whether to compile a function definition to λ□.
shouldCompileFunction :: Definition -> Bool
shouldCompileFunction def@Defn{theDef} | Function{..} <- theDef
  = not (theDef ^. funInline) -- not inlined (from module application)
    && isNothing funExtLam    -- not a pattern-lambda-generated function (inlined by the treeless translation)
    && isNothing funWith      -- not a with-generated function           (inlined by the treeless translation)
    && hasQuantityω def       -- non-erased

type MutualGraph = Map QName (Set QName)

-- | Detect the mutually-recursive group of a function via treeless traversal.
--
-- NB: see the discussion on Agda issue #8111 on why @funMutual@ is not enough,
-- i.e. the positivity checker misses some calls on agruments it deems unused.
recursiveGroup :: Definition -> TCM [QName]
recursiveGroup Defn{defName, defMutual = block} =
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
  calls q = treeless q >>= \case
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
compileFunction t defn | not (shouldCompileFunction defn) = pure Nothing
compileFunction (t :: Target t) defn@Defn{defType} = do
  mnames <- liftTCM $ recursiveGroup defn
  mdefs  <- liftTCM $ mapM getConstInfo mnames
  reportSDoc "agda2lambox.compile.function" 5 $
    "Recursive group:" <+> prettyTCM mnames
  -- (conditionally) compile type of function
  typ <- whenTyped t $ case isRecordProjection (theDef defn) of
    Nothing -> compileTopLevelType defType

    -- if it is a (real) projection, drop the parameters from the type
    Just (recName, _) -> do
      Record{recPars} <- fmap theDef $ liftTCM $ getConstInfo recName
      projTel <- telViewUpTo recPars defType
      projType <- theCore <$> telViewUpTo recPars defType
      let names  = map domName $ toList $ theTel projTel
          pnames = map (maybe LBox.Anon (LBox.Named . sanitize . prettyShow)) names
      (pnames,) <$> compileType recPars projType

    -- TODO(flupe): ^ take care of projection-like functions
    --                they should be eta-expanded somehow,
    --                OR treated like projections

  let builder :: LBox.Term -> Maybe (LBox.GlobalDecl t)
      builder = Just . ConstantDecl . ConstantBody typ . Just

  -- if the function is not recursive, just compile the body
  if null mdefs then builder <$> compileFunctionBody [] defn

  -- otherwise, take fixpoint
  else do
    let k = fromMaybe 0 $ elemIndex (defName defn) mnames

    builder . flip LBox.LFix k <$>
      forM mdefs \def@Defn{defName} -> do
        body <- compileFunctionBody mnames def >>= \case
          l@LBox.LLambda{} -> pure l
          LBox.LBox        -> pure $ LBox.LLambda LBox.Anon LBox.LBox
          _                -> genericError "Fixpoint body must be Lambda."
        return LBox.Def
          { dName = qnameToName defName
          , dBody = body
          , dArgs = 0
          }
