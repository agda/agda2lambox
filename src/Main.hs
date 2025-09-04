{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE GADTs, BangPatterns #-}
-- | The agda2lambox Agda backend
module Main (main) where

import Control.Monad ( unless, filterM )
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData(rnf) )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.Version ( showVersion )
import Data.Text ( pack )
import GHC.Generics ( Generic )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(..) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), takeDirectory )
import Data.Text.Lazy.IO qualified as LText

import Paths_agda2lambox ( version )

import Agda.Compiler.Common
import Agda.Compiler.Backend
import Agda.Main ( runAgda )
import Agda.Syntax.Internal ( clauseWhereModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.Syntax.Common.Pretty ( pretty, prettyShow )

import Agda.Utils ( pp, hasPragma )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile       (compile)
import CoqGen    ( prettyCoq  )
import SExpr     ( prettySexp )
import LambdaBox.Env
import Agda2Lambox.Compile.Monad (runCompile, CompileEnv(..))


main :: IO ()
main = runAgda [agda2lambox]

data Output = RocqOutput | AstOutput
  deriving (Eq, Show, Generic, NFData)

-- | Backend options.
data Options = forall t. Options
  { optOutDir   :: Maybe FilePath
  , optTarget   :: Target t
  , optOutput   :: Output
  , optNoBlocks :: Bool
  }

instance NFData Options where
  rnf (Options m t o nb) = rnf m `seq` rnf t `seq` rnf o `seq` rnf nb

-- | Setter for output directory option.
outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts { optOutDir = Just dir }

typedOpt :: Monad m => Options -> m Options
typedOpt opts = return opts { optTarget = ToTyped }

rocqOpt :: Monad m => Options -> m Options
rocqOpt opts = return opts { optOutput = RocqOutput }

noBlocksOpt :: Monad m => Options -> m Options
noBlocksOpt opts = return opts { optNoBlocks = True }

-- | Default backend options.
defaultOptions :: Options
defaultOptions  = Options
  { optOutDir   = Nothing
  , optTarget   = ToUntyped
  , optOutput   = AstOutput
  , optNoBlocks = False
  }

-- | Backend module environments.
type ModuleEnv = ()
type ModuleRes = ()

-- | The adga2lambox backend.
agda2lambox :: Backend
agda2lambox = Backend backend
  where
    backend :: Backend' Options Options ModuleEnv ModuleRes QName
    backend = Backend'
      { backendName           = "agda2lambox"
      , backendInteractTop    = Nothing
      , backendInteractHole   = Nothing
      , backendVersion        = Just $ pack $ showVersion version
      , options               = defaultOptions
      , commandLineFlags      =
          [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
            "Write output files to DIR. (default: project root)"
          , Option ['t'] ["typed"] (NoArg typedOpt) 
            "Compile to typed λ□ environments."
          , Option ['c'] ["rocq"] (NoArg rocqOpt) 
            "Output a Rocq file."
          , Option [] ["no-blocks"] (NoArg noBlocksOpt) 
            "Disable constructors as blocks."
          ]
      , isEnabled             = \ _ -> True
      , preCompile            = return
      , postCompile           = \ _ _ _ -> return ()
      , preModule             = moduleSetup
      , postModule            = writeModule
      , compileDef            = \ _ _ _ -> pure . defName
      , scopeCheckingSuffices = False
      , mayEraseType          = \ _ -> return True
      }

moduleSetup
  :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
  -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ NotMain m _ = pure $ Skip ()
moduleSetup _ IsMain m _ = do
  setScope . iInsideScope =<< curIF
  pure $ Recompile ()

writeModule
  :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
  -> [QName]
  -> TCM ModuleRes
writeModule opts menv NotMain _ _   = pure ()
writeModule Options{..} menv IsMain m defs = do
  outDir   <- flip fromMaybe optOutDir <$> compileDir
  env      <- runCompile (CompileEnv optNoBlocks) $ compile optTarget defs
  programs <- filterM hasPragma defs

  let fileName = (outDir </>) . moduleNameToFileName m
      coqMod   = CoqModule env (map qnameToKName programs)

  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fileName ""

  liftIO do
    putStrLn $ "Writing " <> fileName ".txt"
    pp coqMod <> "\n" & writeFile (fileName ".txt")

  liftIO $ case optOutput of
    RocqOutput -> do
      putStrLn $ "Writing " <> fileName ".v"
      prettyCoq optTarget coqMod <> "\n"
        & writeFile (fileName ".v")

    AstOutput -> do
      putStrLn $ "Writing " <> fileName ".ast"
      prettySexp optTarget coqMod <> "\n"
        & LText.writeFile (fileName ".ast")
