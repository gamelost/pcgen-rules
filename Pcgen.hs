module Main where

import System.Environment(getArgs)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe(mapMaybe)
import Pcc
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
  firstPcc <- parsePCC $ head args
  results <- constructPCC firstPcc
  print results
