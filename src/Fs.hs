{-# LANGUAGE OverloadedStrings #-}

module Fs (getFullPath, Location(..)) where

import Control.Monad (forM, msum)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import Prelude

data Location = Data | Vendor | Any deriving Show

-- Link prefix syntax does not seem to follow the PCGen documentation
-- (no extant samples use '&'), and futhermore there seem to be some
-- links that don't have a location prefix. So screw it: let's just
-- recursively look for the filename.

getFullPath :: FilePath -> IO FilePath
getFullPath x = do
  fullPath <- findFile "." $ takeFileName x
  case fullPath of
    Just f -> return f
    Nothing -> error $ "referenced file " ++ show x ++ " could not be found"

findFile :: FilePath -> FilePath -> IO (Maybe FilePath)
findFile location target = do
  names <- getDirectoryContents location
  let validFiles = filter (`notElem` [".", ".."]) names
  results <- forM validFiles $ \name -> do
    let path = location </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then
      findFile path target
    else
      return (if name == target then Just path else Nothing)
  return $ msum results
