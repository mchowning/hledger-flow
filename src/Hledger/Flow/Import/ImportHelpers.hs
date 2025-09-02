{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (findInputCSVs, findJournalFiles, groupIncludesUpTo, includeFileName, discoverAccountDirs, extractAccountComponents, extractAccountComponentsUnsafe, accountDirToAbsDir, absToAccountDir, validateAccountDirPath, validateAccountStructure, categorizeAccount, validateAllAccounts, includeYearFilesForParent, dirToStringNoSlash) where

import Path
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
import System.FilePath (dropTrailingPathSeparator, splitDirectories)
import Text.Read (readMaybe)

import Hledger.Flow.Common (groupValuesBy, concatMapM)
import Hledger.Flow.PathHelpers (AbsDir, AbsFile, RelDir, RelFile, findFilesIn, pathSize)
import Hledger.Flow.Import.Types (InputFileBundle)
import Hledger.Flow.Types (AccountDir(..), AccountValidation(..), AccountType(..))
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import qualified System.Directory as Dir
import Control.Monad (filterM)

findInputCSVs :: Integer -> AbsDir -> IO [AbsFile]
findInputCSVs startYear = do
  let excludeDirs = [[reldir|2-preprocessed|], [reldir|3-journal|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [reldir|1-in|] startYear) excludeDirs

-- Because of the commonExcludeDirs, this excludes anything in the _manual_ directory
findJournalFiles :: AbsDir -> IO [AbsFile]
findJournalFiles = do
  let excludeDirs = [[reldir|1-in|], [reldir|2-preprocessed|]] ++ commonExcludeDirs
  findFilesIn (includeYearFilesForParent [reldir|3-journal|] 0) excludeDirs

-- | Include only files directly underneath parentDir/yearDir, e.g. 1-in/2020/* or 3-journal/2020/*
includeYearFilesForParent :: RelDir -> Integer -> AbsDir -> Bool
includeYearFilesForParent parentDir startYear d = (dirname . parent) d == parentDir
  && length shortDirName == 4
  && all isDigit shortDirName
  && maybe False (>= startYear) (readMaybe shortDirName)
    where shortDirName = dirToStringNoSlash d

dirToStringNoSlash :: AbsDir -> String
dirToStringNoSlash d = case Path.toFilePath (Path.dirname d) of
  ""  -> ""   -- Handle empty path case
  str -> init str  -- Remove trailing slash safely

commonExcludeDirs :: [RelDir]
commonExcludeDirs = [[reldir|_manual_|], [reldir|__pycache__|]]

groupIncludesUpTo :: RelDir -> [RelFile] -> InputFileBundle
groupIncludesUpTo = groupIncludesUpTo' Map.empty

groupIncludesUpTo' :: InputFileBundle -> RelDir -> [RelFile] -> InputFileBundle
groupIncludesUpTo' acc _ [] = acc
groupIncludesUpTo' acc stopAt journals = do
  let dirs = map parent journals :: [RelDir]
  let shouldStop = stopAt `elem` dirs
  if shouldStop then acc else do
    let grouped = groupIncludeFilesPerYear journals
    groupIncludesUpTo' (acc <> grouped) stopAt (Map.keys grouped)

groupIncludeFilesPerYear :: [RelFile] -> InputFileBundle
groupIncludeFilesPerYear [] = Map.empty
groupIncludeFilesPerYear ps@(p:_) = if pathSize (parent p) == 6
  then groupValuesBy initialIncludeFilePath ps
  else groupValuesBy parentIncludeFilePath ps

initialIncludeFilePath :: RelFile -> RelFile
initialIncludeFilePath p = (parent . parent . parent) p </> includeFileName p

parentIncludeFilePath :: RelFile -> RelFile
parentIncludeFilePath p = (parent . parent) p </> filename p

includeFileName :: RelFile -> RelFile
includeFileName f = do
  let includeFile = (dropTrailingPathSeparator . toFilePath . dirname . parent) f ++ "-include.journal"
  fromMaybe [relfile|unknown-include.journal|] $ parseRelFile includeFile


-- | Discover all account directories following the import/{owner}/{institution}/{account}/ pattern.
-- This function provides the foundation for account-centric processing, replacing the old
-- CSV-first discovery approach. The input directory can be either the base directory (which contains import/)
-- or the import directory itself.
discoverAccountDirs :: AbsDir -> IO [AccountDir]
discoverAccountDirs inputDir = do
  -- Check if inputDir is already the import directory or if we need to append 'import'
  let possibleImportDir = inputDir </> [reldir|import|]
  isInputDirImport <- Dir.doesDirectoryExist (toFilePath possibleImportDir)
  let importDir = if isInputDirImport then possibleImportDir else inputDir

  importExists <- Dir.doesDirectoryExist (toFilePath importDir)
  if not importExists
    then return []
    else do
      -- Find all subdirectories at the import/{owner}/{institution}/{account}/ level
      accountPaths <- findAccountPaths importDir
      -- Validate paths using absToAccountDir and filter out invalid ones
      return $ mapMaybe absToAccountDir accountPaths

findAccountPaths :: AbsDir -> IO [AbsDir]
findAccountPaths importDir = do
  ownerDirs <- listSubdirectories importDir
  institutionDirs <- concatMapM listSubdirectories ownerDirs
  accountDirs <- concatMapM listSubdirectories institutionDirs
  return accountDirs

listSubdirectories :: AbsDir -> IO [AbsDir]
listSubdirectories dir = do
  exists <- Dir.doesDirectoryExist (toFilePath dir)
  if not exists
    then return []
    else do
      contents <- Dir.listDirectory (toFilePath dir)
      -- Filter out hidden files/directories (starting with '.') before parsing
      let visibleContents = filter (not . isHiddenEntry) contents
      -- Parse directory names first, then check if they exist
      let parsedPaths = mapMaybe parseRelDir visibleContents
      let fullPaths = map (dir </>) parsedPaths
      -- Filter to only actual directories
      filterM (Dir.doesDirectoryExist . toFilePath) fullPaths

-- | Check if a directory entry should be considered hidden
isHiddenEntry :: FilePath -> Bool
isHiddenEntry name = case name of
  ""        -> True   -- Empty names are invalid
  ('.':_)   -> True   -- Hidden files/directories start with '.'
  _         -> False


-- | Extract owner, institution, and account name components from an AccountDir.
-- Returns Just (owner, institution, account) as Text values for valid paths.
-- Returns Nothing if the path structure doesn't match the expected pattern.
extractAccountComponents :: AccountDir -> Maybe (T.Text, T.Text, T.Text)
extractAccountComponents (AccountDir accountDir) =
  let pathComponents = splitDirectories (toFilePath accountDir)
  in case reverse pathComponents of
       (account:institution:owner:"import":_) ->
         if not (null account) && not (null institution) && not (null owner)
           then Just (T.pack owner, T.pack institution, T.pack account)
           else Nothing
       _ -> Nothing

-- | Extract account components safely, returning empty strings for invalid paths.
-- This function provides backward compatibility for existing code.
extractAccountComponentsUnsafe :: AccountDir -> (T.Text, T.Text, T.Text)
extractAccountComponentsUnsafe accountDir =
  case extractAccountComponents accountDir of
    Just result -> result
    Nothing -> (T.empty, T.empty, T.empty)

-- | Convert an AccountDir back to an AbsDir for use with existing path functions
accountDirToAbsDir :: AccountDir -> AbsDir
accountDirToAbsDir (AccountDir absDir) = absDir

-- | Convert an AbsDir to an AccountDir with path validation.
-- Returns Nothing if the path doesn't follow the import/{owner}/{institution}/{account} pattern.
absToAccountDir :: AbsDir -> Maybe AccountDir
absToAccountDir absDir =
  if validateAccountDirPath absDir
    then Just (AccountDir absDir)
    else Nothing

-- | Validate that an AbsDir follows the import/{owner}/{institution}/{account} pattern.
-- Checks that the path has the correct depth and structure.
validateAccountDirPath :: AbsDir -> Bool
validateAccountDirPath absDir =
  let pathComponents = splitDirectories (toFilePath absDir)
  in case reverse pathComponents of
       (account:institution:owner:"import":_) ->
         not (null account) && not (null institution) && not (null owner)
       _ -> False

-- | Validate an account directory's structure and categorize it by data source type.
-- Checks for the presence of 1-in/ (CSV data) and _manual_/ (manual journal files)
-- directories to determine if the account is CSV-only, manual-only, or mixed.
validateAccountStructure :: AccountDir -> IO AccountValidation
validateAccountStructure (AccountDir accountDir) = do
  let csvDir = accountDir </> [reldir|1-in|]
  let manualDir = accountDir </> [reldir|_manual_|]

  csvExists <- Dir.doesDirectoryExist (toFilePath csvDir)
  manualExists <- Dir.doesDirectoryExist (toFilePath manualDir)

  case (csvExists, manualExists) of
    (True, True) -> return $ ValidAccount Mixed
    (True, False) -> return $ ValidAccount CSVOnly
    (False, True) -> return $ ValidAccount ManualOnly
    (False, False) -> return $ InvalidAccount (T.pack "Account contains neither 1-in/ nor _manual_/ directory")

-- | Categorize an account based on its directory structure.
-- Returns Nothing for invalid accounts that contain neither CSV nor manual data sources.
-- This function provides the account type information needed for processing decisions.
categorizeAccount :: AccountDir -> IO (Maybe AccountType)
categorizeAccount accountDir = do
  validation <- validateAccountStructure accountDir
  case validation of
    ValidAccount accountType -> return (Just accountType)
    InvalidAccount _ -> return Nothing

-- | Validate all discovered accounts and return their validation results.
-- Useful for batch validation and reporting structural issues before processing begins.
validateAllAccounts :: [AccountDir] -> IO [(AccountDir, AccountValidation)]
validateAllAccounts accounts = do
  validations <- mapM validateAccountStructure accounts
  return $ zip accounts validations
