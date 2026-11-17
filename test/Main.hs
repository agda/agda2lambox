-- | Compiles every test module to untyped λ□ in test/dist/ and typed λ□ in test/dist/typed/,
-- checking that each program has a main term and, if `peregrine` is installed, that it validates.
-- Also writes test/Tests.agda, which imports all of them.
module Main (main) where

import Control.Monad (forM, when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf, sort)
import System.Directory (findExecutable, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (-<.>), dropExtension, takeExtension)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

main :: IO ()
main = do
  peregrine <- findExecutable "peregrine"
  setCurrentDirectory "test"
  dist <- (</> "dist") <$> getCurrentDirectory
  tests <- forM ["agda2lambox", "agda2rust", "agda2hs"] \d ->
    (d,) . sort . filter ((== ".agda") . takeExtension) <$> listDirectory d
  writeFile "Tests.agda" $ unlines
    [ l | (d, fs) <- tests, l <- ("-- " ++ d) : map (("open import " ++) . dropExtension) fs ]
  defaultMain $ testGroup "agda2lambox"
    [ testGroup d
      [ testCase (unwords $ (d </> f) : flags) do
          run d "agda2lambox" (flags ++ ["-o", out, f])
          let ast = out </> f -<.> "ast"
          prog <- readFile ast
          when ("None)" `isSuffixOf` dropWhileEnd isSpace prog) $
            assertFailure $ "no main program in " ++ ast
          mapM_ (\p -> run "." p ["validate", ast]) peregrine
      | f <- fs, (out, flags) <- [(dist, ["--rocq"]), (dist </> "typed", ["--typed", "--rocq"])] ]
    | (d, fs) <- tests ]
  where
  run dir cmd args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc cmd args) { cwd = Just dir } ""
    when (code /= ExitSuccess) $ assertFailure $ unwords (cmd : args) ++ "\n" ++ out ++ err
