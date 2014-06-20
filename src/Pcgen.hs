{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment(getArgs)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe(mapMaybe)
import System.FilePath(splitExtension)
import Pcc
import Lst
import Fs

type PCCMap = M.Map T.Text [T.Text]

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
    linkTags (PCCBodyTag l) = Just (getFullPath . T.unpack $ filename l)
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
  let inputFilename = head args
  case snd $ splitExtension inputFilename of
    d | d == ".lst" ->
      case T.pack $ args !! 1 of
        "LANGUAGE" -> do
          lFile <- parseLanguageLST inputFilename
          print lFile
        "ARMORPROF" -> do
          aFile <- parseArmorProfLST inputFilename
          print aFile
        _ -> do
          gFile <- parseGenericLST inputFilename
          print gFile
    d | d == ".pcc" -> do
      firstPcc <- parsePCC inputFilename
      results <- constructPCC firstPcc
      print results
    _ ->
      error "Error: no valid filename supplied"
