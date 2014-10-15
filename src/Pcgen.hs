{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import qualified Text.Show.Pretty as Pretty

import System.Environment(getArgs)
import Data.Maybe (mapMaybe)
import System.FilePath (splitExtension)
import Prelude

import Pcc
import Lst
import Fs

type PCCMap = M.Map String [String]

data Ruleset = Ruleset { config :: PCCMap
                       , links :: [Ruleset]} deriving Show

constructPCCMap :: PCCTags -> PCCMap
constructPCCMap pccTags =
  M.fromListWith (++) tags where
    tags = mapMaybe dataTags pccTags
    dataTags (PCCDataTag x y) = Just (x, [y])
    dataTags _ = Nothing

constructPCCLinks :: PCCTags -> IO [PCCTags]
constructPCCLinks pccTags =
  mapM (>>= parsePCC) tags where
    tags = mapMaybe linkTags pccTags
    linkTags (PCCBodyTag l) = Just (getFullPath $ filename l)
    linkTags _ = Nothing

constructPCC :: PCCTags -> IO Ruleset
constructPCC result = do
  let pccConfig = constructPCCMap result
  pccLinks <- constructPCCLinks result
  rulesets <- mapM constructPCC pccLinks
  return Ruleset { config = pccConfig
                 , links = rulesets }

-- testing, for now just print out what we have
main :: IO ()
main = do
  args <- getArgs
  inputFilename <- getFullPath $ head args
  case snd $ splitExtension inputFilename of
    d | d == ".lst" -> do
      results <- parseLSTToString (args !! 1) inputFilename
      putStrLn results
    d | d == ".pcc" -> do
      firstPcc <- parsePCC inputFilename
      results <- constructPCC firstPcc
      putStrLn $ Pretty.ppShow results
    _ ->
      error "Error: no valid filename supplied"
