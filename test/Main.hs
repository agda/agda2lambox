{-# LANGUAGE OverloadedStrings #-}
-- | Test suite for agda2lambox using tasty
module Main (main) where

import Control.Monad (filterM, unless)
import System.Directory
import System.FilePath
import System.Process

import System.Exit (ExitCode(..))
import Data.List (sort)

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))

main :: IO ()
main = do
  lboxPath <- findExecutable "lbox"

  -- Check if lbox executable exists (optional)
  case lboxPath of
    Nothing -> putStrLn "Warning: lbox executable not found. AST validation will be skipped."
    Just _ -> return ()

  -- Save the original directory and change to test/
  originalDir <- getCurrentDirectory
  setCurrentDirectory "test"

  -- Create a temporary directory for build outputs
  tempDir <- getTemporaryDirectory
  let buildDir = tempDir </> "agda2lambox-test-build"
  createDirectoryIfMissing True buildDir

  -- Discover files: first untyped, then typed
  untypedFiles <- listAgdaBasenames "untyped"
  typedFiles   <- listAgdaBasenames "typed"

  let untypedTests = map (mkAgdaTest lboxPath buildDir "untyped" False) untypedFiles
      typedTests   = map (mkAgdaTest lboxPath buildDir "typed" True) typedFiles

  defaultMain $ testGroup "agda2lambox tests"
    [ testGroup "untyped" untypedTests
    , testGroup "typed" typedTests
    ]

-- List basenames of .agda files in a subdirectory (returns [] if dir missing)
listAgdaBasenames :: FilePath -> IO [FilePath]
listAgdaBasenames subdir = do
  exists <- doesDirectoryExist subdir
  if not exists
    then return []
    else do
      files <- listDirectory subdir
      let agdaFiles = filter (\f -> takeExtension f == ".agda" && not (endsWith "~" f)) files
      return $ sort agdaFiles
  where
    endsWith suffix str = suffix `elem` words (reverse (take (length suffix) (reverse str)))


-- Create a test case for a single .agda file located in `subdir`.
-- subdir: "untyped" or "typed"; baseFile is the filename within that dir.
mkAgdaTest :: Maybe FilePath -> FilePath -> FilePath -> Bool -> FilePath -> TestTree
mkAgdaTest mLboxPath buildDir subdir isTyped baseFile = testCase ("Test: " ++ (subdir </> baseFile)) $ do
  let astTarget = buildDir </> baseFile -<.> ".ast"

  -- Step 1: Run agda2lambox on the file (run with cwd=subdir so Agda sees basename)
  let typedFlag = ["--typed" | isTyped]
      cp = (proc "agda2lambox" (typedFlag ++ ["-o", buildDir, baseFile])) { cwd = Just subdir }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode cp ""

  case exitCode of
    ExitFailure code -> do
      assertFailure $
        "agda2lambox failed with exit code " ++ show code ++ "\n" ++
        "stdout: " ++ stdout ++ "\n" ++
        "stderr: " ++ stderr

    ExitSuccess -> do
      -- Check that the .ast file was created
      astExists <- doesFileExist astTarget
      unless astExists $
        assertFailure $ "Expected AST file not created: " ++ astTarget

      -- Step 2: Validate the AST file with lbox validate if available
      case mLboxPath of
        Nothing -> return ()  -- lbox not available, skip validation
        Just lboxPath -> do
          let validateArgs = ["validate"] ++ ["--typed=true" | isTyped] ++ [astTarget]
          (validateExitCode, validateStdout, validateStderr) <-
            readProcessWithExitCode lboxPath validateArgs ""

          case validateExitCode of
            ExitFailure vcode -> do
              assertFailure $
                "lbox validate failed with exit code " ++ show vcode ++ "\n" ++
                "File: " ++ astTarget ++ "\n" ++
                "stdout: " ++ validateStdout ++ "\n" ++
                "stderr: " ++ validateStderr

            ExitSuccess -> return ()
