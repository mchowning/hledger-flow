module CSVImport.Unit where

import Test.HUnit

import qualified CSVImport.ImportHelperTests
import qualified CSVImport.ImportHelperTurtleTests
import qualified AccountDiscovery.Unit

tests :: Test
tests = TestList [CSVImport.ImportHelperTests.tests, CSVImport.ImportHelperTurtleTests.tests, AccountDiscovery.Unit.tests]
