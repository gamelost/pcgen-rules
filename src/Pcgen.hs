{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment(getArgs)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Show.Pretty as Pretty
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
  inputFilename <- getFullPath $ head args
  case snd $ splitExtension inputFilename of
    d | d == ".lst" ->
      case T.pack $ args !! 1 of
        "LANGUAGE" -> do
          lFile <- parseLanguageLST inputFilename
          putStrLn $ Pretty.ppShow lFile
        "ARMORPROF" -> do
          aFile <- parseArmorLST inputFilename
          putStrLn $ Pretty.ppShow aFile
        "SHIELDPROF" -> do
          sFile <- parseShieldLST inputFilename
          putStrLn $ Pretty.ppShow sFile
        "WEAPONPROF" -> do
          wFile <- parseWeaponLST inputFilename
          putStrLn $ Pretty.ppShow wFile
        "SKILL" -> do
          sFile <- parseSkillLST inputFilename
          putStrLn $ Pretty.ppShow sFile
        _ -> do
          gFile <- parseGenericLST inputFilename
          putStrLn $ Pretty.ppShow gFile
    d | d == ".pcc" -> do
      firstPcc <- parsePCC inputFilename
      results <- constructPCC firstPcc
      putStrLn $ Pretty.ppShow results
    _ ->
      error "Error: no valid filename supplied"
