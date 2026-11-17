{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
-- | The agda2lambox Agda backend
module Main (main) where

import Control.Monad ( filterM, when )
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData(rnf) )
import Data.Maybe ( fromMaybe )
import Data.Version ( showVersion )
import Data.Text ( pack )
import Agda.Utils.GetOpt ( OptDescr(Option), ArgDescr(..) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (-<.>) )
import Data.Text.Lazy.IO qualified as LText

import Paths_agda2lambox ( version )

import Agda.Compiler.Common
import Agda.Compiler.Backend
import Agda.Main ( runAgda )
import Agda.Syntax.Common.Pretty ( prettyShow )

import Agda.Utils ( hasPragma )
import Agda.Utils.Treeless ( unfoldSelfProjections )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile ( compile )
import Agda2Lambox.Compile.Expand ( expandModule )
import CodeGen.Rocq ( prettyRocq )
import CodeGen.SExpr ( prettySexp )
import LambdaBox.Env
import Agda2Lambox.Compile.Monad ( runCompile, genericError )


main :: IO ()
main = runAgda [agda2lambox]

-- | Backend options.
data Options = forall t. Options
  { optOutDir  :: Maybe FilePath
  , optTarget  :: Target t
  , optRocq    :: Bool
  , optLibrary :: Bool
  }

instance NFData Options where
  rnf (Options m t r l) = rnf m `seq` rnf t `seq` rnf r `seq` rnf l

-- | The agda2lambox backend.
agda2lambox :: Backend
agda2lambox = Backend backend
  where
    backend :: Backend' Options Options () () QName
    backend = Backend'
      { backendName           = "agda2lambox"
      , backendInteractTop    = Nothing
      , backendInteractHole   = Nothing
      , backendVersion        = Just $ pack $ showVersion version
      , options               = Options Nothing ToUntyped False False
      , commandLineFlags      =
          [ Option ['o'] ["out-dir"] (ReqArg (\ dir o -> pure o{optOutDir = Just dir}) "DIR")
            "Write output files to DIR. (default: project root)"
          , Option ['t'] ["typed"] (NoArg \ o -> pure o{optTarget = ToTyped})
            "Compile to typed λ□ environments."
          , Option ['c'] ["rocq"] (NoArg \ o -> pure o{optRocq = True})
            "Output a Rocq file."
          , Option ['a'] ["export-all"] (NoArg \ o -> pure o{optLibrary = True})
            "Compile every definition of the module, not only those reachable from a COMPILE pragma (to extract a library)."
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
  -> TCM (Recompile () ())
moduleSetup _ NotMain _ _ = pure $ Skip ()
moduleSetup _ IsMain  _ _ = Recompile () <$ (setScope . iInsideScope =<< curIF)

writeModule :: Options -> () -> IsMain -> TopLevelModuleName -> [QName] -> TCM ()
writeModule _ _ NotMain _ _ = pure ()
writeModule Options{..} _ IsMain m defs = do
  outDir   <- flip fromMaybe optOutDir <$> compileDir
  programs <- filterM hasPragma defs

  when (null programs && not optLibrary) $ genericError
    "No main program specified. Please use a COMPILE pragma, or --export-all to extract a library."

  unfoldSelfProjections

  -- only compile what is reachable from the programs, unless extracting a library
  env   <- runCompile $ compile optTarget $ if optLibrary then defs else programs
  mains <- mapM mainProgram programs

  let lboxMod = expandModule $ LBoxModule env mains
      out :: String -> (FilePath -> s -> IO ()) -> s -> IO ()
      out ext w s = do
        let f = (outDir </> prettyShow m) -<.> ext
        putStrLn $ "Writing " <> f
        w f s

  liftIO do
    createDirectoryIfMissing True outDir
    out ".txt" writeFile $ prettyShow lboxMod <> "\n"
    out ".ast" LText.writeFile $ prettySexp optTarget lboxMod <> "\n"
    when optRocq $ out ".v" writeFile $ prettyRocq optTarget lboxMod <> "\n"
