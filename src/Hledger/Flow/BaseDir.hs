{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.BaseDir (
    determineBaseDir
  , relativeToBase
  , relativeToBase'
  , turtleBaseDir
  , effectiveRunDir
) where

import Path
import Path.IO
import Hledger.Flow.Types (HasBaseDir, BaseDir, RunDir, baseDir)
import Hledger.Flow.PathHelpers

import Data.Maybe

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when)

import qualified Turtle (stripPrefix)

determineBaseDir :: Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir suppliedDir = do
  pwd <- getCurrentDir
  determineBaseDir' pwd suppliedDir

determineBaseDir' :: AbsDir -> Maybe TurtlePath -> IO (BaseDir, RunDir)
determineBaseDir' pwd (Just suppliedDir) = do
  absDir <- turtleToAbsDir pwd suppliedDir
  determineBaseDirFromStartDir absDir
determineBaseDir' pwd Nothing = determineBaseDirFromStartDir pwd

determineBaseDirFromStartDir ::  AbsDir -> IO (BaseDir, RunDir)
determineBaseDirFromStartDir startDir = determineBaseDirFromStartDir' startDir startDir

determineBaseDirFromStartDir' :: (MonadIO m, MonadThrow m) => AbsDir -> AbsDir -> m (BaseDir, RunDir)
determineBaseDirFromStartDir' startDir possibleBaseDir = do
  Control.Monad.when (parent possibleBaseDir == possibleBaseDir) $ throwM (MissingBaseDir startDir)
  foundBaseDir <- doesDirExist $ possibleBaseDir </> [reldir|import|]
  if foundBaseDir then
    do
      runDir <- limitRunDir possibleBaseDir startDir
      return (possibleBaseDir, runDir)
    else determineBaseDirFromStartDir' startDir $ parent possibleBaseDir

-- | Limit runDir depth to prevent include file generation issues.
-- Depths 0-4 (base through account level) are passed through unchanged.
-- Depths greater than 4 are limited back to account level (depth 4) to prevent
-- include files being generated in wrong locations and journals written outside baseDir.
limitRunDir :: (MonadIO m, MonadThrow m) => BaseDir -> AbsDir -> m RunDir
limitRunDir bd absRunDir = do
  rel <- makeRelative bd absRunDir
  let runDirDepth = pathSize rel
  -- Only limit depths greater than 4 back to account level (depth 4)
  let fun = composeN (runDirDepth - 4) parent
  let newRunDir = fun rel
  return newRunDir

composeN :: Int -> (a -> a) -> (a -> a)
composeN n f | n < 1      = id
             | n == 1     = f
             | otherwise = composeN (n-1) (f . f)

relativeToBase :: HasBaseDir o => o -> TurtlePath -> TurtlePath
relativeToBase opts = relativeToBase' $ pathToTurtle (baseDir opts)

relativeToBase' :: TurtlePath -> TurtlePath -> TurtlePath
relativeToBase' bd p = if forceTrailingSlash bd == forceTrailingSlash p then "./" else
  fromMaybe p $ Turtle.stripPrefix (forceTrailingSlash bd) p

turtleBaseDir :: HasBaseDir o => o -> TurtlePath
turtleBaseDir opts = pathToTurtle $ baseDir opts

effectiveRunDir :: BaseDir -> RunDir -> AbsDir
effectiveRunDir bd rd = do
  let baseImportDir = bd </> [Path.reldir|import|]
  let absRunDir = bd </> rd
  if absRunDir == bd then baseImportDir else absRunDir
