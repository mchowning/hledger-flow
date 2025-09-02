{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AccountDiscovery.Unit where

import Test.HUnit
import Path
import qualified Turtle
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)
import TestHelpers
import Hledger.Flow.PathHelpers (fromTurtleAbsDir)
import Hledger.Flow.Types (AccountDir(..), AccountValidation(..), AccountType(..))
import Hledger.Flow.Import.ImportHelpers (discoverAccountDirs, extractAccountComponents, extractAccountComponentsUnsafe, accountDirToAbsDir, absToAccountDir, validateAccountStructure, categorizeAccount, validateAllAccounts, includeYearFilesForParent, dirToStringNoSlash)

testDiscoverAccountDirsEmpty :: Test
testDiscoverAccountDirsEmpty = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    result <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Empty directory should return no accounts" [] result
  )

testDiscoverAccountDirsSingleAccount :: Test
testDiscoverAccountDirsSingleAccount = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account structure: import/john/bigbank/checking/
    let accountPath = tmpdir Turtle.</> "import/john/bigbank/checking"
    Turtle.mktree accountPath

    result <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover single account" 1 (length result)

    -- Verify we can extract components from the discovered account
    let accountDir = head result
    let (owner, institution, account) = extractAccountComponentsUnsafe accountDir
    liftIO $ assertEqual "Owner should be 'john'" "john" owner
    liftIO $ assertEqual "Institution should be 'bigbank'" "bigbank" institution
    liftIO $ assertEqual "Account should be 'checking'" "checking" account
  )

testDiscoverAccountDirsMultipleAccounts :: Test
testDiscoverAccountDirsMultipleAccounts = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create multiple account structures
    let account1 = tmpdir Turtle.</> "import/alice/bank1/checking"
    let account2 = tmpdir Turtle.</> "import/alice/bank1/savings"
    let account3 = tmpdir Turtle.</> "import/bob/bank2/investment"
    Turtle.mktree account1
    Turtle.mktree account2
    Turtle.mktree account3

    result <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover all three accounts" 3 (length result)
  )

testDiscoverAccountDirsIgnoresNonAccountPaths :: Test
testDiscoverAccountDirsIgnoresNonAccountPaths = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let validAccount = tmpdir Turtle.</> "import/jane/mybank/savings"
    Turtle.mktree validAccount

    -- Create invalid paths that should be ignored
    let tooShallow = tmpdir Turtle.</> "import/jane"  -- missing bank/account
    let tooDeep = tmpdir Turtle.</> "import/jane/mybank/savings/extra/deep"
    let wrongPrefix = tmpdir Turtle.</> "notimport/jane/mybank/checking"
    Turtle.mktree tooShallow
    Turtle.mktree tooDeep
    Turtle.mktree wrongPrefix

    result <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should only discover valid account paths" 1 (length result)
  )

testAccountDirConversions :: Test
testAccountDirConversions = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account structure
    let accountPath = tmpdir Turtle.</> "import/bob/testbank/current"
    Turtle.mktree accountPath

    result <- liftIO $ discoverAccountDirs tmpdirAbsPath
    let accountDir = head result

    -- Test conversions
    let absDir = accountDirToAbsDir accountDir
    let maybeBackToAccountDir = absToAccountDir absDir

    case maybeBackToAccountDir of
      Just backToAccountDir ->
        liftIO $ assertEqual "Round-trip conversion should preserve AccountDir" accountDir backToAccountDir
      Nothing ->
        liftIO $ assertFailure "Valid account directory should convert back successfully"
  )

testAbsToAccountDirValidPath :: Test
testAbsToAccountDirValidPath = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/bob/testbank/current"
    Turtle.mktree accountPath

    -- Convert to AbsDir and validate
    accountPathAbs <- fromTurtleAbsDir accountPath
    let result = absToAccountDir accountPathAbs

    -- Should succeed for valid path
    case result of
      Just accountDir -> do
        let (owner, institution, account) = extractAccountComponentsUnsafe accountDir
        liftIO $ assertEqual "Owner should be extracted correctly" "bob" owner
        liftIO $ assertEqual "Institution should be extracted correctly" "testbank" institution
        liftIO $ assertEqual "Account should be extracted correctly" "current" account
      Nothing -> liftIO $ assertFailure "Valid account path should be accepted"
  )

testAbsToAccountDirInvalidPathTooShallow :: Test
testAbsToAccountDirInvalidPathTooShallow = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create path that's too shallow (missing components)
    let shallowPath = tmpdir Turtle.</> "import/bob"
    Turtle.mktree shallowPath

    -- Convert to AbsDir and validate
    shallowPathAbs <- fromTurtleAbsDir shallowPath
    let result = absToAccountDir shallowPathAbs

    -- Should fail for invalid path
    case result of
      Just _ -> liftIO $ assertFailure "Invalid path (too shallow) should be rejected"
      Nothing -> return () -- Expected behavior
  )

testAbsToAccountDirInvalidPathWrongPattern :: Test
testAbsToAccountDirInvalidPathWrongPattern = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create path that doesn't follow import/{owner}/{institution}/{account} pattern
    let wrongPath = tmpdir Turtle.</> "export/bob/testbank/current"
    Turtle.mktree wrongPath

    -- Convert to AbsDir and validate
    wrongPathAbs <- fromTurtleAbsDir wrongPath
    let result = absToAccountDir wrongPathAbs

    -- Should fail for wrong pattern
    case result of
      Just _ -> liftIO $ assertFailure "Path not following import pattern should be rejected"
      Nothing -> return () -- Expected behavior
  )

testAbsToAccountDirEmptyComponents :: Test
testAbsToAccountDirEmptyComponents = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create path with empty component (double slash)
    let emptyPath = tmpdir Turtle.</> "import/bob//current"
    Turtle.mktree emptyPath

    -- Convert to AbsDir and validate
    emptyPathAbs <- fromTurtleAbsDir emptyPath
    let result = absToAccountDir emptyPathAbs

    -- Should fail for empty components
    case result of
      Just _ -> liftIO $ assertFailure "Path with empty components should be rejected"
      Nothing -> return () -- Expected behavior
  )

testExtractAccountComponentsValidPath :: Test
testExtractAccountComponentsValidPath = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/alice/testbank/checking"
    Turtle.mktree accountPath

    accountPathAbs <- fromTurtleAbsDir accountPath
    case absToAccountDir accountPathAbs of
      Just accountDir -> do
        let result = extractAccountComponents accountDir
        case result of
          Just (owner, institution, account) -> do
            liftIO $ assertEqual "Owner should be extracted correctly" "alice" owner
            liftIO $ assertEqual "Institution should be extracted correctly" "testbank" institution
            liftIO $ assertEqual "Account should be extracted correctly" "checking" account
          Nothing -> liftIO $ assertFailure "Valid account path should extract components successfully"
      Nothing -> liftIO $ assertFailure "Valid account path should be accepted"
  )

testExtractAccountComponentsInvalidPath :: Test
testExtractAccountComponentsInvalidPath = TestCase (
  do
    -- Create an AccountDir with an invalid structure (this shouldn't happen with validated construction)
    -- But we can test the function directly with a manually created invalid path
    let invalidPath = [absdir|/some/invalid/path|]
    let invalidAccountDir = AccountDir invalidPath
    let result = extractAccountComponents invalidAccountDir

    case result of
      Nothing -> return () -- Expected behavior for invalid paths
      Just _ -> assertFailure "Invalid account path should not extract components successfully"
  )

testIncludeYearFilesForParentValidYear :: Test
testIncludeYearFilesForParentValidYear = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create a valid year directory structure
    let yearPath = tmpdir Turtle.</> "1-in/2020"
    Turtle.mktree yearPath

    yearPathAbs <- fromTurtleAbsDir yearPath
    let result = includeYearFilesForParent [reldir|1-in|] 2020 yearPathAbs
    liftIO $ assertBool "Should include valid year directory" result
  )

testIncludeYearFilesForParentInvalidYear :: Test
testIncludeYearFilesForParentInvalidYear = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create an invalid year directory (non-numeric)
    let invalidPath = tmpdir Turtle.</> "1-in/abcd"
    Turtle.mktree invalidPath

    invalidPathAbs <- fromTurtleAbsDir invalidPath
    let result = includeYearFilesForParent [reldir|1-in|] 2020 invalidPathAbs
    liftIO $ assertBool "Should reject non-numeric year directory" (not result)
  )

testDirToStringNoSlashNormal :: Test
testDirToStringNoSlashNormal = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")

    -- Create a normal directory
    let normalPath = tmpdir Turtle.</> "test/directory"
    Turtle.mktree normalPath

    normalPathAbs <- fromTurtleAbsDir normalPath
    let result = dirToStringNoSlash normalPathAbs
    liftIO $ assertBool "Should handle normal directory" (not (null result))
  )

testValidateAccountStructureCSVOnly :: Test
testValidateAccountStructureCSVOnly = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account with only 1-in/ directory
    let accountPath = tmpdir Turtle.</> "import/bob/testbank/checking"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    result <- liftIO $ validateAccountStructure accountDir
    liftIO $ assertEqual "Account with only 1-in/ should be ValidAccount CSVOnly" (ValidAccount CSVOnly) result
  )

testValidateAccountStructureManualOnly :: Test
testValidateAccountStructureManualOnly = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account with only _manual_/ directory
    let accountPath = tmpdir Turtle.</> "import/alice/bankone/savings"
    let manualDir = accountPath Turtle.</> "_manual_"
    Turtle.mktree manualDir

    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    result <- liftIO $ validateAccountStructure accountDir
    liftIO $ assertEqual "Account with only _manual_/ should be ValidAccount ManualOnly" (ValidAccount ManualOnly) result
  )

testValidateAccountStructureMixed :: Test
testValidateAccountStructureMixed = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account with both 1-in/ and _manual_/ directories
    let accountPath = tmpdir Turtle.</> "import/charlie/mixedbank/current"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let manualDir = accountPath Turtle.</> "_manual_"
    Turtle.mktree csvInputDir
    Turtle.mktree manualDir

    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    result <- liftIO $ validateAccountStructure accountDir
    liftIO $ assertEqual "Account with both 1-in/ and _manual_/ should be ValidAccount Mixed" (ValidAccount Mixed) result
  )

testValidateAccountStructureInvalid :: Test
testValidateAccountStructureInvalid = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account with neither 1-in/ nor _manual_/ directories
    let accountPath = tmpdir Turtle.</> "import/dana/emptybank/account"
    Turtle.mktree accountPath

    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    result <- liftIO $ validateAccountStructure accountDir
    case result of
      InvalidAccount _ -> liftIO $ assertBool "Account with no data sources should be InvalidAccount" True
      _ -> liftIO $ assertFailure "Expected InvalidAccount for account with no data sources"
  )

testValidateAllAccounts :: Test
testValidateAllAccounts = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create multiple accounts with different structures
    let csvAccount = tmpdir Turtle.</> "import/owner1/bank1/csv-account/1-in"
    let manualAccount = tmpdir Turtle.</> "import/owner2/bank2/manual-account/_manual_"
    let mixedAccount1 = tmpdir Turtle.</> "import/owner3/bank3/mixed-account/1-in"
    let mixedAccount2 = tmpdir Turtle.</> "import/owner3/bank3/mixed-account/_manual_"
    let invalidAccount = tmpdir Turtle.</> "import/owner4/bank4/invalid-account"

    Turtle.mktree csvAccount
    Turtle.mktree manualAccount
    Turtle.mktree mixedAccount1
    Turtle.mktree mixedAccount2
    Turtle.mktree invalidAccount

    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    results <- liftIO $ validateAllAccounts accounts

    liftIO $ assertEqual "Should have 4 accounts discovered" 4 (length results)
    -- Verify that we have some valid and some invalid accounts
    let validCount = length [() | (_, ValidAccount _) <- results]
    let invalidCount = length [() | (_, InvalidAccount _) <- results]
    liftIO $ assertEqual "Should have 3 valid accounts" 3 validCount
    liftIO $ assertEqual "Should have 1 invalid account" 1 invalidCount
  )

testAccountDiscoveryIgnoresDeepSubdirectories :: Test
testAccountDiscoveryIgnoresDeepSubdirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: A valid account structure with deep subdirectories
    let accountDir = tmpdir Turtle.</> "import/owner/bank/account"
    let deepSubDir = accountDir Turtle.</> "subdir1/subdir2/subdir3"
    Turtle.mktree accountDir
    Turtle.mktree deepSubDir

    -- When: I discover accounts
    result <- liftIO $ discoverAccountDirs tmpdirAbsPath

    -- Then: Only the account-level directory should be discovered
    liftIO $ assertEqual "Should find exactly one account" 1 (length result)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head result)
    liftIO $ assertEqual "Should extract owner correctly" "owner" owner
    liftIO $ assertEqual "Should extract institution correctly" "bank" institution
    liftIO $ assertEqual "Should extract account name correctly" "account" account
  )

testAccountDiscoveryHandlesSpecialCharacters :: Test
testAccountDiscoveryHandlesSpecialCharacters = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: Account directories with common special characters
    let hyphenatedAccount = tmpdir Turtle.</> "import/john-doe/big_bank/checking-01"
    let dottedAccount = tmpdir Turtle.</> "import/jane.smith/bank.co/savings_account"
    Turtle.mktree hyphenatedAccount
    Turtle.mktree dottedAccount

    -- When: I discover accounts
    result <- liftIO $ discoverAccountDirs tmpdirAbsPath

    -- Then: Both accounts should be discovered
    liftIO $ assertEqual "Should discover both accounts with special characters" 2 (length result)

    -- And: Component extraction should work correctly
    let components = map extractAccountComponentsUnsafe result
    liftIO $ assertBool "Should correctly parse names with hyphens, dots, and underscores"
      (("john-doe", "big_bank", "checking-01") `elem` components &&
       ("jane.smith", "bank.co", "savings_account") `elem` components)
  )

testAccountDiscoveryIgnoresIrrelevantSubdirectories :: Test
testAccountDiscoveryIgnoresIrrelevantSubdirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: An account with CSV data directory and some irrelevant subdirectories
    let accountPath = tmpdir Turtle.</> "import/user/mybank/checking"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let randomDir1 = accountPath Turtle.</> "temp-files"
    let randomDir2 = accountPath Turtle.</> "backup"
    Turtle.mktree csvInputDir
    Turtle.mktree randomDir1
    Turtle.mktree randomDir2

    -- When: I discover and validate accounts
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath

    -- Then: The account should be discovered and remain valid
    liftIO $ assertEqual "Should discover the account" 1 (length accounts)

    validation <- liftIO $ validateAccountStructure (head accounts)
    liftIO $ assertEqual "Account should be valid CSV-only type" (ValidAccount CSVOnly) validation
  )

testAccountValidationWithProcessingDirectories :: Test
testAccountValidationWithProcessingDirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: An account with full CSV processing pipeline directories
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let preprocessedDir = accountPath Turtle.</> "2-preprocessed"
    let journalDir = accountPath Turtle.</> "3-journal"
    Turtle.mktree csvInputDir
    Turtle.mktree preprocessedDir
    Turtle.mktree journalDir

    -- When: I validate the account
    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    validation <- liftIO $ validateAccountStructure accountDir

    -- Then: It should be valid CSV-only (processing dirs don't affect classification)
    liftIO $ assertEqual "Account with processing directories should be CSV-only" (ValidAccount CSVOnly) validation
  )

testAccountValidationWithCustomScripts :: Test
testAccountValidationWithCustomScripts = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: An account with CSV data and custom processing scripts
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let preprocessScript = accountPath Turtle.</> "preprocess"
    let constructScript = accountPath Turtle.</> "construct"
    Turtle.mktree csvInputDir
    Turtle.touch preprocessScript
    Turtle.touch constructScript

    -- When: I validate the account
    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    validation <- liftIO $ validateAccountStructure accountDir

    -- Then: Custom scripts should not affect CSV-only classification
    liftIO $ assertEqual "Account with custom scripts should still be CSV-only" (ValidAccount CSVOnly) validation
  )

testAccountValidationWithEmptyDataDirectories :: Test
testAccountValidationWithEmptyDataDirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: An account with empty 1-in/ and _manual_/ directories
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let manualDir = accountPath Turtle.</> "_manual_"
    Turtle.mktree csvInputDir
    Turtle.mktree manualDir

    -- When: I validate the account
    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    validation <- liftIO $ validateAccountStructure accountDir

    -- Then: Empty directories still indicate a Mixed account type
    liftIO $ assertEqual "Account with empty data directories should be Mixed" (ValidAccount Mixed) validation
  )

testAccountValidationIgnoresAdditionalFiles :: Test
testAccountValidationIgnoresAdditionalFiles = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Given: A manual-only account with various additional files that should be ignored
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let manualDir = accountPath Turtle.</> "_manual_"
    let rulesFile = accountPath Turtle.</> "bank-account.rules"
    let readmeFile = accountPath Turtle.</> "README.txt"
    let configFile = accountPath Turtle.</> "config.yml"
    Turtle.mktree manualDir
    Turtle.touch rulesFile
    Turtle.touch readmeFile
    Turtle.touch configFile

    -- When: I validate the account
    [accountDir] <- liftIO $ discoverAccountDirs tmpdirAbsPath
    validation <- liftIO $ validateAccountStructure accountDir

    -- Then: Additional files should not affect manual-only classification
    liftIO $ assertEqual "Additional files should not affect validation" (ValidAccount ManualOnly) validation
  )

testDiscoverAccountDirsWithFilesAtImportLevel :: Test
testDiscoverAccountDirsWithFilesAtImportLevel = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/user/mybank/checking"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    -- Add files at the import level (similar to your actual structure)
    let importDir = tmpdir Turtle.</> "import"
    let journalFile1 = importDir Turtle.</> "2024-include.journal"
    let journalFile2 = importDir Turtle.</> "all-years.journal"
    Turtle.touch journalFile1
    Turtle.touch journalFile2

    -- Test that discovery still works despite files at import level
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover account despite files at import level" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should extract components correctly" ("user", "mybank", "checking") (owner, institution, account)
  )

testDiscoverAccountDirsWithFilesAtOwnerLevel :: Test
testDiscoverAccountDirsWithFilesAtOwnerLevel = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/personal/bank/checking"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    -- Add files at the owner level (like in your personal directory)
    let ownerDir = tmpdir Turtle.</> "import/personal"
    let journalFile1 = ownerDir Turtle.</> "2024-include.journal"
    let journalFile2 = ownerDir Turtle.</> "2024-opening.journal"
    let journalFile3 = ownerDir Turtle.</> "all-years.journal"
    Turtle.touch journalFile1
    Turtle.touch journalFile2
    Turtle.touch journalFile3

    -- Test that discovery works despite files at owner level
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover account despite files at owner level" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should extract components correctly" ("personal", "bank", "checking") (owner, institution, account)
  )

testDiscoverAccountDirsWithFilesAtInstitutionLevel :: Test
testDiscoverAccountDirsWithFilesAtInstitutionLevel = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/owner/institution/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    -- Add files at the institution level
    let institutionDir = tmpdir Turtle.</> "import/owner/institution"
    let rulesFile = institutionDir Turtle.</> "institution.rules"
    let configFile = institutionDir Turtle.</> "config.yaml"
    Turtle.touch rulesFile
    Turtle.touch configFile

    -- Test that discovery works despite files at institution level
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover account despite files at institution level" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should extract components correctly" ("owner", "institution", "account") (owner, institution, account)
  )

testDiscoverAccountDirsWithNonStandardNames :: Test
testDiscoverAccountDirsWithNonStandardNames = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create accounts with names that might cause parseRelDir issues
    let account1 = tmpdir Turtle.</> "import/user-1/bank.com/account_1"
    let account2 = tmpdir Turtle.</> "import/123user/bank-name/2024-account"
    let csvInput1 = account1 Turtle.</> "1-in"
    let csvInput2 = account2 Turtle.</> "1-in"
    Turtle.mktree csvInput1
    Turtle.mktree csvInput2

    -- Test that discovery handles non-standard directory names
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover accounts with non-standard names" 2 (length accounts)

    let components = map extractAccountComponentsUnsafe accounts
    liftIO $ assertBool "Should handle special characters in names"
      (("user-1", "bank.com", "account_1") `elem` components &&
       ("123user", "bank-name", "2024-account") `elem` components)
  )

testDiscoverAccountDirsWithSymbolicLinks :: Test
testDiscoverAccountDirsWithSymbolicLinks = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/owner/bank/checking"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    -- Create a symbolic link (this might fail on some systems, that's OK)
    let linkPath = tmpdir Turtle.</> "import/owner/bank/link-to-checking"
    liftIO $ (Turtle.symlink accountPath linkPath) `catch` (\(_ :: SomeException) -> return ())

    -- Test that discovery handles symbolic links gracefully
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertBool "Should discover at least the real account" (length accounts >= 1)

    -- Should find the real account
    let realAccount = filter (\acc ->
          let (_, _, name) = extractAccountComponentsUnsafe acc
          in name == "checking") accounts
    liftIO $ assertEqual "Should find the real checking account" 1 (length realAccount)
  )

testDiscoverAccountDirsWithHiddenDirectories :: Test
testDiscoverAccountDirsWithHiddenDirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account structure
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    Turtle.mktree csvInputDir

    -- Add hidden directories and files at various levels
    let importHidden = tmpdir Turtle.</> "import/.git"
    let ownerHidden = tmpdir Turtle.</> "import/owner/.DS_Store"
    let bankHidden = tmpdir Turtle.</> "import/owner/bank/.cache"
    Turtle.mktree importHidden
    Turtle.touch ownerHidden
    Turtle.mktree bankHidden

    -- Test that discovery ignores hidden directories/files
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should discover account while ignoring hidden files" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should extract components correctly" ("owner", "bank", "account") (owner, institution, account)
  )

testDiscoverAccountDirsWithDeepNestedStructures :: Test
testDiscoverAccountDirsWithDeepNestedStructures = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create account with deep nested subdirectories (like statements/2024/01/)
    let accountPath = tmpdir Turtle.</> "import/owner/bank/account"
    let csvInputDir = accountPath Turtle.</> "1-in"
    let statementsDir = accountPath Turtle.</> "statements/2024/01/very/deep/structure"
    Turtle.mktree csvInputDir
    Turtle.mktree statementsDir

    -- Test that discovery stops at account level, ignoring deep structures
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should find only the account level directory" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should extract account level components" ("owner", "bank", "account") (owner, institution, account)
  )

testDiscoverAccountDirsWithEmptyDirectories :: Test
testDiscoverAccountDirsWithEmptyDirectories = TestCase (
  Turtle.sh $ do
    currentDir <- Turtle.pwd
    tmpdir <- Turtle.using (Turtle.mktempdir currentDir "hlflow")
    tmpdirAbsPath <- fromTurtleAbsDir tmpdir

    -- Create valid account
    let validAccount = tmpdir Turtle.</> "import/owner1/bank1/account1/1-in"
    Turtle.mktree validAccount

    -- Create empty owner and institution directories
    let emptyOwner = tmpdir Turtle.</> "import/owner2"
    let emptyInstitution = tmpdir Turtle.</> "import/owner3/bank3"
    Turtle.mktree emptyOwner
    Turtle.mktree emptyInstitution

    -- Test that discovery handles empty intermediate directories gracefully
    accounts <- liftIO $ discoverAccountDirs tmpdirAbsPath
    liftIO $ assertEqual "Should find only valid account, ignore empty directories" 1 (length accounts)

    let (owner, institution, account) = extractAccountComponentsUnsafe (head accounts)
    liftIO $ assertEqual "Should find the valid account" ("owner1", "bank1", "account1") (owner, institution, account)
  )

tests :: Test
tests = TestList [
    testDiscoverAccountDirsEmpty,
    testDiscoverAccountDirsSingleAccount,
    testDiscoverAccountDirsMultipleAccounts,
    testDiscoverAccountDirsIgnoresNonAccountPaths,
    testAccountDirConversions,
    testAbsToAccountDirValidPath,
    testAbsToAccountDirInvalidPathTooShallow,
    testAbsToAccountDirInvalidPathWrongPattern,
    testAbsToAccountDirEmptyComponents,
    testExtractAccountComponentsValidPath,
    testExtractAccountComponentsInvalidPath,
    testIncludeYearFilesForParentValidYear,
    testIncludeYearFilesForParentInvalidYear,
    testDirToStringNoSlashNormal,
    testValidateAccountStructureCSVOnly,
    testValidateAccountStructureManualOnly,
    testValidateAccountStructureMixed,
    testValidateAccountStructureInvalid,
    testValidateAllAccounts,
    testAccountDiscoveryIgnoresDeepSubdirectories,
    testAccountDiscoveryHandlesSpecialCharacters,
    testAccountDiscoveryIgnoresIrrelevantSubdirectories,
    testAccountValidationWithProcessingDirectories,
    testAccountValidationWithCustomScripts,
    testAccountValidationWithEmptyDataDirectories,
    testAccountValidationIgnoresAdditionalFiles,
    testDiscoverAccountDirsWithFilesAtImportLevel,
    testDiscoverAccountDirsWithFilesAtOwnerLevel,
    testDiscoverAccountDirsWithFilesAtInstitutionLevel,
    testDiscoverAccountDirsWithNonStandardNames,
    testDiscoverAccountDirsWithSymbolicLinks,
    testDiscoverAccountDirsWithHiddenDirectories,
    testDiscoverAccountDirsWithDeepNestedStructures,
    testDiscoverAccountDirsWithEmptyDirectories
  ]