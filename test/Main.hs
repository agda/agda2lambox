-- | Test suite for agda2lambox using tasty
module Main (main) where

import Control.Monad (unless, when, forM)
import System.Directory
import System.FilePath
import System.Process

import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import Data.List (sort, isSuffixOf, dropWhileEnd)

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)

main :: IO ()
main = do
  peregrinePath <- findExecutable "peregrine"

  -- Check if peregrine executable exists (optional)
  case peregrinePath of
    Nothing -> putStrLn "Warning: peregrine executable not found. AST validation will be skipped."
    Just _ -> return ()

  -- Save the original directory and change to test/
  originalDir <- getCurrentDirectory
  let testDir = originalDir </> "test"
  setCurrentDirectory testDir

  -- Create a temporary directory for build outputs
  let buildDir = testDir </> "dist"
  createDirectoryIfMissing True buildDir

  -- Discover files
  let testFolders = ["agda2lambox", "agda2rust", "agda2hs"]
  files <- forM testFolders \d -> (d,) <$> listAgdaBasenames d

  -- Create index file
  writeFile (testDir </> "Tests.agda")
     $ unlines
     $ flip concatMap files \(d, fs) ->
          ("-- " ++ d) : map (("open import " <>) . dropExtension) fs

  -- Create tests
  let typedDir = buildDir </> "typed"
      modes    = [(buildDir, ["--rocq"]), (typedDir, ["--typed", "--rocq"])]
  defaultMain $ testGroup "agda2lambox tests" $
    flip map files \(d, fs) -> testGroup d
      [ mkAgdaTest peregrinePath out d flags f | f <- fs, (out, flags) <- modes ]


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


-- Create a test case in `buildDir` for a single .agda file located in `subdir`.
mkAgdaTest :: Maybe FilePath -> FilePath -> FilePath -> [String] -> FilePath -> TestTree
mkAgdaTest mperegrinePath buildDir subdir flags baseFile =
  testCase (unwords $ "Test:" : (subdir </> baseFile) : flags) $ do
  let astTarget = buildDir </> baseFile -<.> ".ast"

  -- Step 1: Run agda2lambox on the file (run with cwd=subdir so Agda sees basename)
  let cp = (proc "agda2lambox" (flags ++ ["-o", buildDir, baseFile]))
            { cwd = Just subdir }
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

      -- Check that the program carries its main term
      ast <- readFile astTarget
      when ("None)" `isSuffixOf` dropWhileEnd isSpace ast) $
        assertFailure $ "AST file has no main program: " ++ astTarget

      -- Step 2: Validate the AST file with peregrine validate if available
      case mperegrinePath of
        Nothing -> return ()  -- peregrine not available, skip validation
        Just peregrinePath -> do
          (validateExitCode, validateStdout, validateStderr) <-
            readProcessWithExitCode peregrinePath ["validate", astTarget] ""

          case validateExitCode of
            ExitFailure vcode -> do
              assertFailure $
                "peregrine validate failed with exit code " ++ show vcode ++ "\n" ++
                "File: " ++ astTarget ++ "\n" ++
                "stdout: " ++ validateStdout ++ "\n" ++
                "stderr: " ++ validateStderr

            ExitSuccess -> return ()
