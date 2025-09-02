{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions for include file generation supporting both CSV and manual account types.
-- Uses TurtlePath for compatibility with existing ecosystem. The account-centric functions
-- provide better support for manual-only accounts without requiring placeholder files.
module Hledger.Flow.Import.ImportHelpersTurtle (
    allYearIncludeFiles
  , extractImportDirs
  , extraIncludesForFile
  , generateIncludesFromAccounts
  , groupAndWriteIncludeFilesFromAccounts
  , groupIncludeFiles
  , groupAndWriteIncludeFiles
  , includePreamble
  , toIncludeFiles
  , toIncludeLine
  , writeIncludesUpTo
  , writeIncludesUpToFromAccounts
  , writeToplevelAllYearsInclude
  , yearsIncludeMap
  )
 where

import Hledger.Flow.PathHelpers (TurtlePath, AbsFile, AbsDir)
import Hledger.Flow.DocHelpers (docURL)
import Hledger.Flow.Common (allYearsFileName, directivesFile, filterPaths, groupValuesBy, writeFiles, writeFiles', concatMapM)
import Hledger.Flow.BaseDir (relativeToBase, relativeToBase', turtleBaseDir)
import Hledger.Flow.Logging (logVerbose)

import Hledger.Flow.Types (AccountDir, AccountType(..), HasBaseDir(..), HasVerbosity(..), LogMessage)
import Hledger.Flow.Import.Types
import Hledger.Flow.Import.ImportHelpers (accountDirToAbsDir, findJournalFiles, categorizeAccount)

import qualified Data.List as List (nub, sort)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import qualified Turtle
import Turtle ((%), (</>), (<.>))

import Control.Concurrent.STM (TChan)
import Data.Maybe (fromMaybe)
import Path (toFilePath, reldir)
import qualified Path
import qualified System.Directory as Dir
import Hledger.Flow.PathHelpers (findFilesIn)

-- | Extract import directory structure from a file path within an account.
-- Validates the expected import/{owner}/{institution}/{account}/{filestate}/{year} structure.
extractImportDirs :: TurtlePath -> Either T.Text ImportDirs
extractImportDirs inputFile = do
  case importDirBreakdown inputFile of
    [bd,owner,bank,account,filestate,year] -> Right $ ImportDirs bd owner bank account filestate year
    _ -> do
      Left $ Turtle.format ("I couldn't find the right number of directories between \"import\" and the input file:\n" % Turtle.fp
                      % "\n\nhledger-flow expects to find input files in this structure:\n" %
                      "import/owner/bank/account/filestate/year/trxfile\n\n" %
                      "Have a look at the documentation for a detailed explanation:\n" % Turtle.s) inputFile (docURL "input-files")

importDirBreakdown ::  TurtlePath -> [TurtlePath]
importDirBreakdown = importDirBreakdown' []

importDirBreakdown' :: [TurtlePath] -> TurtlePath -> [TurtlePath]
importDirBreakdown' acc path = do
  let dir = Turtle.directory path
  if Turtle.dirname dir == "import" || (Turtle.dirname dir == "")
    then dir:acc
    else importDirBreakdown' (dir:acc) $ Turtle.parent dir

groupIncludeFiles :: [TurtlePath] -> (TurtleFileBundle, TurtleFileBundle)
groupIncludeFiles = allYearIncludeFiles . groupIncludeFilesPerYear . filter isJournalFile

isJournalFile :: TurtlePath -> Bool
isJournalFile f = Turtle.extension f == Just "journal"

allYearIncludeFiles :: TurtleFileBundle -> (TurtleFileBundle, TurtleFileBundle)
allYearIncludeFiles m = (m, yearsIncludeMap $ Map.keys m)

yearsIncludeMap :: [TurtlePath] -> TurtleFileBundle
yearsIncludeMap = groupValuesBy allYearsPath

allYearsPath :: TurtlePath -> TurtlePath
allYearsPath = allYearsPath' Turtle.directory

allYearsPath' :: (TurtlePath -> TurtlePath) -> TurtlePath -> TurtlePath
allYearsPath' dir p = dir p </> allYearsFileName

includeFileName :: TurtlePath -> TurtlePath
includeFileName = (<.> "journal") . T.unpack . (Turtle.format (Turtle.fp%"-include")) . Turtle.dirname

groupIncludeFilesPerYear :: [TurtlePath] -> TurtleFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = case extractImportDirs p of
    Right _ -> groupValuesBy initialIncludeFilePath ps
    Left  _ -> groupValuesBy parentIncludeFilePath ps

initialIncludeFilePath :: TurtlePath -> TurtlePath
initialIncludeFilePath p = (Turtle.parent . Turtle.parent . Turtle.parent) p </> includeFileName p

parentIncludeFilePath :: TurtlePath -> TurtlePath
parentIncludeFilePath p = (Turtle.parent . Turtle.parent) p </> Turtle.filename p

toIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtleFileBundle -> IO (Map.Map TurtlePath T.Text)
toIncludeFiles opts ch m = do
  -- Opening files should come first, then pre-import adjustments
  openingMap <- extraIncludes opts ch (Map.keys m) ["opening.journal"] [] []
  preMap     <- extraIncludes opts ch (Map.keys m) [] ["pre-import.journal"] []
  postMap    <- extraIncludes opts ch (Map.keys m) [] ["post-import.journal"] ["prices.journal"]
  closingMap <- extraIncludes opts ch (Map.keys m) ["closing.journal"] [] []
  return $ (addPreamble . toIncludeFiles' openingMap preMap postMap closingMap) m

toIncludeFiles' :: TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> Map.Map TurtlePath T.Text
toIncludeFiles' openingMap preMap postMap closingMap = Map.mapWithKey $ generatedIncludeText openingMap preMap postMap closingMap

addPreamble :: Map.Map TurtlePath T.Text -> Map.Map TurtlePath T.Text
addPreamble = Map.map (\txt -> includePreamble <> "\n" <> txt)

toIncludeLine :: TurtlePath -> TurtlePath -> T.Text
toIncludeLine base file = Turtle.format ("include "%Turtle.fp) $ relativeToBase' base file

generatedIncludeText :: TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> TurtleFileBundle -> TurtlePath -> [TurtlePath] -> T.Text
generatedIncludeText openingMap preMap postMap closingMap outputFile fs = do
  let openingFiles = fromMaybe [] $ Map.lookup outputFile openingMap
  let preFiles = fromMaybe [] $ Map.lookup outputFile preMap
  let files = List.nub . List.sort $ fs
  let postFiles = fromMaybe [] $ Map.lookup outputFile postMap
  let closingFiles = fromMaybe [] $ Map.lookup outputFile closingMap
  let allFiles = List.nub $ openingFiles ++ preFiles ++ files ++ postFiles ++ closingFiles
  let lns = map (toIncludeLine $ Turtle.directory outputFile) allFiles
  T.intercalate "\n" $ lns ++ [""]

includePreamble :: T.Text
includePreamble = "### Generated by hledger-flow - DO NOT EDIT ###\n"

groupAndWriteIncludeFiles :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> IO [TurtlePath]
groupAndWriteIncludeFiles opts ch = writeFileMap opts ch . groupIncludeFiles

writeFileMap :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> (TurtleFileBundle, TurtleFileBundle) -> IO [TurtlePath]
writeFileMap opts ch (m, allYears) = do
  _ <- writeFiles' $ (addPreamble . toIncludeFiles' Map.empty Map.empty Map.empty Map.empty) allYears
  writeFiles . (toIncludeFiles opts ch) $ m

writeIncludesUpTo :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtlePath -> [TurtlePath] -> IO [TurtlePath]
writeIncludesUpTo _ _ _ [] = return []
writeIncludesUpTo opts ch stopAt journalFiles = do
  let shouldStop = any (\dir -> dir == stopAt) $ map Turtle.parent journalFiles
  if shouldStop
    then return journalFiles
    else do
      newJournalFiles <- groupAndWriteIncludeFiles opts ch journalFiles
      writeIncludesUpTo opts ch stopAt newJournalFiles

writeToplevelAllYearsInclude :: (HasBaseDir o, HasVerbosity o) => o -> IO [TurtlePath]
writeToplevelAllYearsInclude opts = do
  directivesExists <- Turtle.testfile (directivesFile opts)
  let preMap = if directivesExists then Map.singleton (turtleBaseDir opts </> allYearsFileName) [directivesFile opts] else Map.empty
  let allTop = Map.singleton (turtleBaseDir opts </> allYearsFileName) ["import" </> allYearsFileName]
  writeFiles' $ (addPreamble . toIncludeFiles' Map.empty preMap Map.empty Map.empty) allTop

extraIncludes :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludes opts ch = extraIncludes' opts ch Map.empty

extraIncludes' :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtleFileBundle -> [TurtlePath] -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludes' _ _ acc [] _ _ _ = return acc
extraIncludes' opts ch acc (file:files) extraSuffixes manualFiles prices = do
  extra <- extraIncludesForFile opts ch file extraSuffixes manualFiles prices
  extraIncludes' opts ch (Map.unionWith (\a b -> List.nub (a ++ b)) acc extra) files extraSuffixes manualFiles prices

-- | Find extra include files (opening, pre-import, post-import, prices, closing) for a given journal file.
-- Used to discover additional manual files that should be included alongside generated journals.
extraIncludesForFile :: (HasVerbosity o, HasBaseDir o) => o -> TChan LogMessage -> TurtlePath -> [T.Text] -> [TurtlePath] -> [TurtlePath] -> IO TurtleFileBundle
extraIncludesForFile opts ch file extraSuffixes manualFiles prices = do
  let dirprefix = T.unpack $ fst $ T.breakOn "-" $ Turtle.format Turtle.fp $ Turtle.basename file
  let fileNames = map (T.unpack . Turtle.format (Turtle.fp % "-" % Turtle.s) dirprefix) extraSuffixes
  let suffixFiles = map (Turtle.directory file </>) fileNames
  let suffixDirFiles = map (((Turtle.directory file </> "_manual_") </> dirprefix) </>) manualFiles
  let priceFiles = map ((((Turtle.directory file </> "..") </> "prices") </> dirprefix) </>) prices
  let extraFiles = suffixFiles ++ suffixDirFiles ++ priceFiles
  filtered <- Turtle.single $ filterPaths Turtle.testfile extraFiles
  let logMsg = Turtle.format ("Looking for possible extra include files for '"%Turtle.fp%"' among these "%Turtle.d%" options: "%Turtle.s%". Found "%Turtle.d%": "%Turtle.s)
               (relativeToBase opts file) (length extraFiles) (Turtle.repr $ relativeFilesAsText opts extraFiles)
               (length filtered) (Turtle.repr $ relativeFilesAsText opts filtered)
  logVerbose opts ch logMsg
  return $ Map.fromList [(file, filtered)]

-- | Find journal files within a single account directory
findJournalFilesInAccount :: AccountDir -> IO [AbsFile]
findJournalFilesInAccount accDir = findJournalFiles (accountDirToAbsDir accDir)

-- | Find manual journal files for accounts that contain manual data
findManualFilesInAccount :: AccountDir -> IO [AbsFile]
findManualFilesInAccount accDir = do
  accountType <- categorizeAccount accDir
  case accountType of
    Just ManualOnly -> findManualJournalFiles (accountDirToAbsDir accDir)
    Just Mixed -> findManualJournalFiles (accountDirToAbsDir accDir)
    _ -> return []

-- | Find all journal files in the _manual_ directory of an account
findManualJournalFiles :: AbsDir -> IO [AbsFile]
findManualJournalFiles accountPath = do
  let manualDir = accountPath Path.</> [reldir|_manual_|]
  manualExists <- Dir.doesDirectoryExist (toFilePath manualDir)
  if manualExists
    then findFilesIn (const True) [] manualDir
    else return []

-- | Generate include files from discovered account directories.
-- This account-centric approach handles CSV-only, manual-only, and mixed accounts uniformly,
-- replacing the need for manual-only account workarounds and placeholder journal files.
generateIncludesFromAccounts :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [AccountDir] -> IO (TurtleFileBundle, TurtleFileBundle)
generateIncludesFromAccounts _opts _ch accounts = do
  -- Find all journal files within the discovered accounts (handles both CSV and manual accounts)
  allJournalFiles <- concatMapM findJournalFilesInAccount accounts
  allManualFiles <- concatMapM findManualFilesInAccount accounts
  -- Convert to turtle paths and use existing grouping logic
  let turtleJournalFiles = map toFilePath (allJournalFiles ++ allManualFiles)
  return $ groupIncludeFiles turtleJournalFiles

-- | Write include files for all discovered accounts, handling mixed account types.
-- Processes both generated journal files (from CSV imports) and existing manual files.
groupAndWriteIncludeFilesFromAccounts :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> [AccountDir] -> IO [TurtlePath]
groupAndWriteIncludeFilesFromAccounts opts ch accounts = do
  (m, allYears) <- generateIncludesFromAccounts opts ch accounts
  writeFileMap opts ch (m, allYears)

-- | Recursively write include files starting from account directories.
-- Handles the complete include file hierarchy for account-centric processing.
writeIncludesUpToFromAccounts :: (HasBaseDir o, HasVerbosity o) => o -> TChan LogMessage -> TurtlePath -> [AccountDir] -> IO [TurtlePath]
writeIncludesUpToFromAccounts _ _ _ [] = return []
writeIncludesUpToFromAccounts opts ch stopAt accounts = do
  newJournalFiles <- groupAndWriteIncludeFilesFromAccounts opts ch accounts
  let shouldStop = any (\dir -> dir == stopAt) $ map Turtle.parent newJournalFiles
  if shouldStop
    then return newJournalFiles
    else do
      -- For recursive processing, we would need to convert back to accounts, but for now
      -- we fall back to the existing file-based approach for higher-level includes
      writeIncludesUpTo opts ch stopAt newJournalFiles

relativeFilesAsText :: HasBaseDir o => o -> [TurtlePath] -> [T.Text]
relativeFilesAsText opts = map (Turtle.format Turtle.fp . relativeToBase opts)
