{-# LANGUAGE OverloadedStrings #-}

module CSVImport
    ( importCSVs
    ) where

import Turtle
import Prelude hiding (FilePath, putStrLn, take)
import Data.Text (breakOnEnd, intercalate, take)
import Data.Text.IO (putStrLn)
import Data.Maybe
import Common

docURL :: Line -> Text
docURL = format ("https://github.com/apauley/hledger-makeitso#"%l)

importCSVs :: FilePath -> IO ()
importCSVs baseDir = do
  let importDir = baseDir </> "import"
  importExists <- testdir importDir
  if importExists
    then
    do
      let importedJournals = importOwners $ lsDirs importDir
      sh $ writeMakeItSoJournal baseDir importedJournals
    else
    do
      let msg = format ("I couldn't find a directory named \"import\" underneath "%fp
                        %"\n\nhledger-makitso expects to find its input files in specifically\nnamed directories.\n\n"%
                        "Have a look at the documentation for a detailed explanation:\n"%s) baseDir (docURL "input-files")
      stderr $ select $ textToLines msg
      exit $ ExitFailure 1

writeMakeItSoJournal :: FilePath -> Shell FilePath -> Shell ()
writeMakeItSoJournal baseDir importedJournals = do
  let importAggregateJournal = baseDir </> "import-all.journal"
  writeJournals importAggregateJournal importedJournals
  let manualDir = baseDir </> "manual"
  let pre = manualDir </> "pre-import.journal"
  let post = manualDir </> "post-import.journal"
  mktree manualDir
  touch pre
  touch post
  let makeitsoJournal = baseDir </> "makeitso.journal"
  writeJournals' dontSort makeitsoJournal $ select [pre, importAggregateJournal, post]

writeJournals :: FilePath -> Shell FilePath -> Shell ()
writeJournals = writeJournals' sort

writeJournals' :: (Shell FilePath -> Shell [FilePath]) -> FilePath -> Shell FilePath -> Shell ()
writeJournals' sortFun aggregateJournal journals = do
  let journalBaseDir = directory aggregateJournal
  liftIO $ writeTextFile aggregateJournal "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n"
  journalFiles <- sortFun journals
  journalFile <- uniq $ select journalFiles
  let strippedJournal = fromMaybe journalFile $ stripPrefix journalBaseDir journalFile
  liftIO $ append aggregateJournal $ toIncludeLines $ return $ strippedJournal

writeJournalsUnsorted :: FilePath -> Shell FilePath -> (Shell FilePath -> Shell [FilePath]) -> Shell ()
writeJournalsUnsorted aggregateJournal journals sortFun = do
  let journalBaseDir = directory aggregateJournal
  liftIO $ writeTextFile aggregateJournal "### Generated by hledger-makeitso - DO NOT EDIT ###\n\n"
  journalFiles <- sortFun journals
  journalFile <- uniq $ select journalFiles
  let strippedJournal = fromMaybe journalFile $ stripPrefix journalBaseDir journalFile
  liftIO $ append aggregateJournal $ toIncludeLines $ return $ strippedJournal

toIncludeLines :: Shell FilePath -> Shell Line
toIncludeLines paths = do
  journalFile <- paths
  return $ fromMaybe "" $ textToLine $ format ("!include "%fp) journalFile

importOwners :: Shell FilePath -> Shell FilePath
importOwners ownerDirs = do
  ownerDir <- ownerDirs
  ownerName <- basenameLine ownerDir
  let ownerJournals = importBanks ownerName $ lsDirs ownerDir
  let aggregateJournal = ownerDir </> buildFilename [ownerName] "journal"
  writeJournals aggregateJournal ownerJournals
  return aggregateJournal

importBanks :: Line -> Shell FilePath -> Shell FilePath
importBanks ownerName bankDirs = do
  bankDir <- bankDirs
  bankName <- basenameLine bankDir
  let bankJournals = importAccounts bankName $ lsDirs bankDir
  let aggregateJournal = bankDir </> buildFilename [bankName] "journal"
  writeJournals aggregateJournal bankJournals
  return aggregateJournal

importAccounts :: Line -> Shell FilePath -> Shell FilePath
importAccounts bankName accountDirs = do
  accDir <- accountDirs
  accName <- basenameLine accDir
  let defaultRulesFile = accDir </> buildFilename [bankName, accName] "rules"
  let preprocessScript = accDir </> fromText "preprocess"
  let constructScript = accDir </> fromText "construct"
  let accountSrcFiles = onlyFiles $ find (has (text "1-in")) accDir
  let accJournals = importAccountFiles bankName accName defaultRulesFile preprocessScript constructScript accountSrcFiles
  let aggregateJournal = accDir </> buildFilename [bankName, accName] "journal"
  let openingJournal = accDir </> "opening.journal"
  liftIO $ touch openingJournal
  writeJournals aggregateJournal $ (return openingJournal) + accJournals
  return aggregateJournal

importAccountFiles :: Line -> Line -> FilePath -> FilePath -> FilePath -> Shell FilePath -> Shell FilePath
importAccountFiles bankName accountName defaultRulesFile preprocessScript constructScript accountSrcFiles = do
  srcFile <- accountSrcFiles
  csvFile <- preprocessIfNeeded preprocessScript bankName accountName srcFile
  doCustomConstruct <- testfile constructScript
  let importFun = if doCustomConstruct
        then customConstruct constructScript bankName accountName
        else hledgerImport defaultRulesFile
  let journalOut = changePathAndExtension "3-journal" "journal" csvFile
  mktree $ directory journalOut
  importFun csvFile journalOut

preprocessIfNeeded :: FilePath -> Line -> Line -> FilePath -> Shell FilePath
preprocessIfNeeded script bank account src = do
  shouldPreprocess <- testfile script
  if shouldPreprocess
    then preprocess script bank account src
    else return src

preprocess :: FilePath -> Line -> Line -> FilePath -> Shell FilePath
preprocess script bank account src = do
  let csvOut = changePathAndExtension "2-preprocessed" "csv" src
  mktree $ directory csvOut
  let script' = format fp script :: Text
  procs script' [format fp src, format fp csvOut, lineToText bank, lineToText account] empty
  return csvOut

hledgerImport :: FilePath -> FilePath -> FilePath -> Shell FilePath
hledgerImport defaultRulesFile csvSrc journalOut = do
  let candidates = rulesFileCandidates csvSrc
  maybeRulesFile <- firstExistingFile candidates
  case maybeRulesFile of
    Just rf -> do
      procs "hledger" ["print", "--rules-file", format fp rf, "--file", format fp csvSrc, "--output-file", format fp journalOut] empty
      return journalOut
    Nothing ->
      do
        let candidatesTxt = intercalate "\n" $ map (format fp) candidates
        let msg = format ("I couldn't find an hledger rules file while trying to import\n"%fp
                          %"\n\nI will happily use the first rules file I can find from any one of these "%d%" files:\n"%s
                          %"\n\nHere is a bit of documentation about rules files that you may find helpful:\n"%s)
                  csvSrc (length candidates) candidatesTxt (docURL "rules-files")
        stderr $ select $ textToLines msg
        exit $ ExitFailure 1

rulesFileCandidates :: FilePath -> [FilePath]
rulesFileCandidates csvSrc = statementSpecificRulesFiles csvSrc ++ generalRulesFiles csvSrc

generalRulesFiles :: FilePath -> [FilePath]
generalRulesFiles csvSrc = do
  let (importDir, ownerDir, bankDir, accountDir, _, _) = dirsRelativeToInputFile csvSrc
  let (owner,bank,account) = ownerBankAcc accountDir

  let accountRulesFile = accountDir </> buildFilename [bank, account] "rules"

  let bankRulesFile = importDir </> buildFilename [bank] "rules"
  [accountRulesFile, bankRulesFile]

statementSpecificRulesFiles :: FilePath -> [FilePath]
statementSpecificRulesFiles csvSrc = do
  let (importDir, ownerDir, bankDir, accountDir, _, _) = dirsRelativeToInputFile csvSrc
  let srcSuffix = snd $ breakOnEnd "_" (format fp (basename csvSrc))

  if ((take 3 srcSuffix) == "rfo")
    then
    do
      let srcSpecificFilename = fromText srcSuffix <.> "rules"
      map (</> srcSpecificFilename) [accountDir, bankDir, importDir]
    else []

dirsRelativeToInputFile :: FilePath -> (FilePath, FilePath, FilePath, FilePath, FilePath, FilePath)
dirsRelativeToInputFile csvSrc = do
  let yearDir = parent csvSrc
  let stateDir = parent yearDir
  let accountDir = parent stateDir
  let bankDir = parent accountDir
  let ownerDir = parent bankDir
  let importDir = parent ownerDir
  (importDir, ownerDir, bankDir, accountDir, stateDir, yearDir)

ownerBankAcc :: FilePath -> (Line, Line, Line)
ownerBankAcc accountDir = do
  let dirs = takeLast 3 $ splitDirectories accountDir
  let dirToLine = firstLine . (format fp) . dirname
  let o:b:a:_ = map dirToLine dirs
  (o,b,a)

customConstruct :: FilePath -> Line -> Line -> FilePath -> FilePath -> Shell FilePath
customConstruct constructScript bank account csvSrc journalOut = do
  let script = format fp constructScript :: Text
  let importOut = inproc script [format fp csvSrc, "-", lineToText bank, lineToText account] empty
  procs "hledger" ["print", "--ignore-assertions", "--file", "-", "--output-file", format fp journalOut] importOut
  return journalOut

changePathAndExtension :: FilePath -> Text -> FilePath -> FilePath
changePathAndExtension newOutputLocation newExt = (changeOutputPath newOutputLocation) . (changeExtension newExt)

changeExtension :: Text -> FilePath -> FilePath
changeExtension extension path = (dropExtension path) <.> extension

changeOutputPath :: FilePath -> FilePath -> FilePath
changeOutputPath newOutputLocation srcFile = mconcat $ map changeSrcDir $ splitDirectories srcFile
  where changeSrcDir f = if (f == "1-in/" || f == "2-preprocessed/") then newOutputLocation else f
