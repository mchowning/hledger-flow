{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module BaseDir.Integration (tests) where

import Prelude hiding (writeFile, readFile)

import Control.Exception (try)
import Test.HUnit

import Path
import Path.IO

import qualified Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Hledger.Flow.Common
import Hledger.Flow.Types (BaseDir, RunDir)
import Hledger.Flow.BaseDir (determineBaseDir)
import Hledger.Flow.PathHelpers

assertSubDirsForDetermineBaseDir :: AbsDir -> BaseDir -> [Path.Path b Dir] -> IO ()
assertSubDirsForDetermineBaseDir initialPwd expectedBaseDir importDirs = do
  sequence_ $ map (assertDetermineBaseDir initialPwd expectedBaseDir) importDirs

assertDetermineBaseDir :: AbsDir -> BaseDir -> Path.Path b Dir -> IO ()
assertDetermineBaseDir initialPwd expectedBaseDir subDir = do
  setCurrentDir initialPwd
  (bd1, runDir1) <- determineBaseDir $ Just $ pathToTurtle subDir
  assertFindTestFileUsingRundir bd1 runDir1

  setCurrentDir subDir
  (bd2, runDir2) <- determineBaseDir Nothing
  assertFindTestFileUsingRundir bd2 runDir2

  (bd3, runDir3) <- determineBaseDir $ Just "."
  assertFindTestFileUsingRundir bd3 runDir3

  (bd4, runDir4) <- determineBaseDir $ Just "./"
  assertFindTestFileUsingRundir bd4 runDir4

  setCurrentDir initialPwd
  let msg dir = "determineBaseDir searches from pwd upwards until it finds a dir containing 'import' - " ++ show dir
  sequence_ $ map (\ dir -> assertEqual (msg dir) expectedBaseDir dir) [bd1, bd2, bd3, bd4]

assertFindTestFileUsingRundir :: BaseDir -> RunDir -> IO ()
assertFindTestFileUsingRundir baseDir runDir = do
  let absRunDir = baseDir </> runDir

  found <- Turtle.single $ fmap (\(x:_) -> x) $ shellToList $ Turtle.find (Turtle.has "test-file.txt") $ pathToTurtle absRunDir
  fileContents <- T.readFile found
  assertEqual "We should find our test file by searching from the returned runDir" (T.pack $ "The expected base dir is " ++ show baseDir) fileContents

assertCurrentDirVariations :: AbsDir -> RelDir -> IO ()
assertCurrentDirVariations absoluteTempDir bdRelativeToTempDir = do
  let absBaseDir = absoluteTempDir </> bdRelativeToTempDir

  setCurrentDir absBaseDir
  (bd1, runDir1) <- determineBaseDir Nothing
  (bd2, runDir2) <- determineBaseDir $ Just "."
  (bd3, runDir3) <- determineBaseDir $ Just "./"
  (bd4, runDir4) <- determineBaseDir $ Just $ pathToTurtle absBaseDir

  let msg label dir = "When pwd is the base dir, determineBaseDir returns the same " ++ label ++ ", regardless of the input variation. " ++ show dir
  sequence_ $ map (\ dir -> assertEqual (msg "baseDir" dir) absBaseDir dir) [bd1, bd2, bd3, bd4]
  sequence_ $ map (\dir -> assertEqual (msg "runDir" dir) [reldir|.|] dir) [runDir1, runDir2, runDir3, runDir4]

testBaseDirWithTempDir :: AbsDir -> AbsDir -> IO ()
testBaseDirWithTempDir initialPwd absoluteTempDir = do
  error1 <- try $ determineBaseDir $ Just "/path/to/dir"
  assertEqual "determineBaseDir produces an error message when given a non-existant dir" (Left $ PathIsNotDirectory "/path/to/dir") error1

  let unrelatedDir = absoluteTempDir </> [reldir|unrelated|]
  createDir unrelatedDir

  bdUnrelated <- try $ determineBaseDir $ Just (pathToTurtle unrelatedDir)
  assertEqual "determineBaseDir produces an error message when it cannot find a baseDir" (Left $ MissingBaseDir unrelatedDir) bdUnrelated

  let baseDir = [reldir|bd1|]
  let importDir = baseDir </> [reldir|import|]
  let ownerDir = importDir </> [reldir|john|]
  let bankDir = ownerDir </> [reldir|mybank|]
  let accDir = bankDir </> [reldir|myacc|]
  let inDir = accDir </> [reldir|1-in|]
  let yearDir = inDir </> [reldir|2019|]
  let subDirs = [yearDir, inDir, accDir, bankDir, ownerDir, importDir, baseDir] :: [RelDir]

  createDirIfMissing True $ absoluteTempDir </> yearDir

  let fictionalDir = absoluteTempDir </> ownerDir </> [reldir|fictionalDir|]
  errorSub <- try $ determineBaseDir $ Just $ pathToTurtle fictionalDir
  assertEqual "determineBaseDir produces an error message when given a non-existant subdir of a valid basedir" (Left $ PathIsNotDirectory $ pathToTurtle fictionalDir) errorSub

  assertCurrentDirVariations absoluteTempDir baseDir

  relativeTempDir <- makeRelative initialPwd absoluteTempDir
  let subDirsRelativeToTop = map (relativeTempDir </>) subDirs
  let absoluteSubDirs = map (absoluteTempDir </>) subDirs

  let absoluteBaseDir = absoluteTempDir </> baseDir

  T.writeFile (pathToTurtle $ absoluteTempDir </> yearDir </> [relfile|test-file.txt|]) (T.pack $ "The expected base dir is " ++ show absoluteBaseDir)

  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir subDirs
  assertSubDirsForDetermineBaseDir absoluteTempDir absoluteBaseDir absoluteSubDirs
  assertSubDirsForDetermineBaseDir initialPwd absoluteBaseDir subDirsRelativeToTop
  return ()

-- | Test runDir depth limiting behavior with the existing test data structure.
-- This maintains compatibility with existing tests while documenting the depth-4 boundary.
assertRunDirs :: RelDir -> [RelDir] -> [RelDir] -> IO ()
assertRunDirs accDir businessAsUsualRundirs specialTreatmentRundirs = do
  sequence_ $ map (assertRunDir id "Depths 0-4 (base through account): passed through unchanged") businessAsUsualRundirs
  sequence_ $ map (assertRunDir (\_ -> accDir) "Depths 5+ (deeper than account): limited back to account level (depth 4)") specialTreatmentRundirs

assertRunDir :: (RelDir -> RelDir) -> String -> RelDir -> IO ()
assertRunDir expectedRunDir msg subDir = do
  (_, runDir) <- determineBaseDir $ Just $ pathToTurtle subDir
  assertEqual msg (expectedRunDir subDir) runDir

testRunDirsWithTempDir :: AbsDir -> IO ()
testRunDirsWithTempDir absoluteTempDir = do
  let baseDir = absoluteTempDir </> [reldir|bd1|]

  -- Create directory structure representing different depth levels
  let importDir = [reldir|import|]                   -- depth 1
  let ownerDir = importDir </> [reldir|john|]        -- depth 2
  let bankDir = ownerDir </> [reldir|mybank|]        -- depth 3
  let accDir = bankDir </> [reldir|myacc|]           -- depth 4 (account level - critical boundary)
  let inDir = accDir </> [reldir|1-in|]              -- depth 5 (should be limited)
  let yearDir = inDir </> [reldir|2019|]             -- depth 6 (should be limited)

  createDirIfMissing True $ baseDir </> yearDir

  -- Test depth limiting: depths 1-4 unchanged, depths 5-6 limited to depth 4
  withCurrentDir baseDir $ assertRunDirs accDir [accDir, bankDir, ownerDir, importDir] [yearDir, inDir]

-- | Test that runDir depth limiting works correctly for valid account directories.
-- Directories deeper than account level (depth 4) should be limited back to account level.
testRunDirDepthLimiting :: Test
testRunDirDepthLimiting = TestCase (
  do
    initialPwd <- getCurrentDir
    let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
    withTempDir tmpbase "hlflowtest" testRunDirsWithTempDir
  )

-- | Test that runDir depth limiting works correctly for all depth levels.
-- This comprehensive test verifies the behavior at each depth level with clear expectations.
testRunDirDepthBehavior :: Test
testRunDirDepthBehavior = TestCase (
  do
    initialPwd <- getCurrentDir
    let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
    withTempDir tmpbase "hlflowtest" testRunDirDepthBehaviorWithTempDir
  )

testRunDirDepthBehaviorWithTempDir :: AbsDir -> IO ()
testRunDirDepthBehaviorWithTempDir absoluteTempDir = do
  let baseDir = absoluteTempDir </> [reldir|bd1|]

  -- Create the full directory structure: import/john/mybank/myacc/1-in/2019/
  let baseLevel = [reldir|.|]                               -- depth 0 (base directory)
  let importDir = [reldir|import|]                          -- depth 1 (import directory)
  let ownerDir = importDir </> [reldir|john|]               -- depth 2 (owner directory)
  let bankDir = ownerDir </> [reldir|mybank|]               -- depth 3 (institution directory)
  let accDir = bankDir </> [reldir|myacc|]                  -- depth 4 (account directory - critical boundary)
  let inDir = accDir </> [reldir|1-in|]                     -- depth 5 (filestate - should be limited)
  let yearDir = inDir </> [reldir|2019|]                    -- depth 6 (year - should be limited)

  createDirIfMissing True $ baseDir </> yearDir

  withCurrentDir baseDir $ do
    -- Test depths 0-4: should pass through unchanged (no limiting applied)
    assertDepthBehavior "Depth 0 (base): passed through unchanged" baseLevel baseLevel
    assertDepthBehavior "Depth 1 (import/): passed through unchanged" importDir importDir
    assertDepthBehavior "Depth 2 (owner/): passed through unchanged" ownerDir ownerDir
    assertDepthBehavior "Depth 3 (institution/): passed through unchanged" bankDir bankDir
    assertDepthBehavior "Depth 4 (account/): passed through unchanged" accDir accDir

    -- Test depths 5+: should be limited back to depth 4 (account level)
    assertDepthBehavior "Depth 5 (1-in/): limited to account level" inDir accDir
    assertDepthBehavior "Depth 6 (year/): limited to account level" yearDir accDir

-- | Assert that a directory depth produces the expected runDir behavior
assertDepthBehavior :: String -> RelDir -> RelDir -> IO ()
assertDepthBehavior msg inputDir expectedRunDir = do
  (_, actualRunDir) <- determineBaseDir $ Just $ pathToTurtle inputDir
  assertEqual msg expectedRunDir actualRunDir

testDetermineBaseDir :: Test
testDetermineBaseDir = TestCase (
  do
    initialPwd <- getCurrentDir
    let tmpbase = initialPwd </> [reldir|test|] </> [reldir|tmp|]
    createDirIfMissing True tmpbase
    withTempDir tmpbase "hlflowtest" $ testBaseDirWithTempDir initialPwd
  )

tests :: Test
tests = TestList [testDetermineBaseDir, testRunDirDepthLimiting, testRunDirDepthBehavior]
