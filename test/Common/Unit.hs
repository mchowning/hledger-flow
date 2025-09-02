{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Common.Unit where

import Test.HUnit

import Hledger.Flow.BaseDir (relativeToBase')
import Hledger.Flow.Common

testShowCmdArgs :: Test
testShowCmdArgs = TestCase (
  do
    let opts = ["--number", "/tmp/file with spaces"]
    let expected = "--number '/tmp/file with spaces'"
    let actual = showCmdArgs opts
    assertEqual "Convert command-line arguments to text" expected actual)

testRelativeToBase :: Test
testRelativeToBase = TestCase (
  do
    let expected = "file1.journal"
    let relativeWithTrailingSlash = relativeToBase' "./base/dir/" "./base/dir/file1.journal"
    assertEqual "relative base dir with trailing slash" expected relativeWithTrailingSlash

    let relativeNoTrailingSlash = relativeToBase' "./base/dir" "./base/dir/file1.journal"
    assertEqual "relative base dir without a trailing slash" expected relativeNoTrailingSlash

    let absoluteWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/file1.journal"
    assertEqual "absolute base dir with trailing slash" expected absoluteWithTrailingSlash

    let absoluteNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir/file1.journal"
    assertEqual "absolute base dir without a trailing slash" expected absoluteNoTrailingSlash

    let absoluteTwiceNoTrailingSlash = relativeToBase' "/base/dir" "/base/dir"
    assertEqual "absolute base dir without a trailing slash supplied twice" "./" absoluteTwiceNoTrailingSlash

    let absoluteTwiceWithTrailingSlash = relativeToBase' "/base/dir/" "/base/dir/"
    assertEqual "absolute base dir with a trailing slash supplied twice" "./" absoluteTwiceWithTrailingSlash

    let absoluteTwiceNoTrailingSlashOnSecondParam = relativeToBase' "/base/dir/" "/base/dir"
    assertEqual "absolute base dir supplied twice, but the second param has no slash" "./" absoluteTwiceNoTrailingSlashOnSecondParam

    let mismatch = relativeToBase' "/base/dir" "/unrelated/dir/file1.journal"
    assertEqual "A basedir with no shared prefix should return the supplied file unchanged" "/unrelated/dir/file1.journal" mismatch
  )

testExtractDigits :: Test
testExtractDigits = TestCase (
  do
    let txt1 = "A number: 321\nAnother number is 42, so is 0"

    let expected1 = Right 321420
    let actual1 = extractDigits txt1
    assertEqual "Extract digits from text 1" expected1 actual1

    let txt2 = "No numbers in this line"

    let expected2 = Left "input does not start with a digit"
    let actual2 = extractDigits txt2
    assertEqual "Extract digits from text 2" expected2 actual2
  )

testConcatMapM :: Test
testConcatMapM = TestCase (
  do
    -- Test that concatMapM correctly concatenates results from monadic functions
    let f x = return [x, x + 10] :: IO [Int]
    result <- concatMapM f [1, 2, 3]
    let expected = [1, 11, 2, 12, 3, 13]
    assertEqual "concatMapM with simple list" expected result

    -- Test empty input list
    emptyResult <- concatMapM f []
    let expectedEmpty = []
    assertEqual "concatMapM with empty list" expectedEmpty emptyResult

    -- Test function that returns empty lists
    let g _ = return [] :: IO [Int]
    emptyFuncResult <- concatMapM g [1, 2, 3]
    let expectedEmptyFunc = []
    assertEqual "concatMapM with function returning empty lists" expectedEmptyFunc emptyFuncResult
  )

tests :: Test
tests = TestList [testShowCmdArgs, testRelativeToBase, testExtractDigits, testConcatMapM]
