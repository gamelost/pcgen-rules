{-# LANGUAGE OverloadedStrings #-}

module Pcc where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Fs
import Util

-- store the path of a given reference file.
data Lookup = Lookup { location :: Location
                     , filename :: T.Text } deriving Show

-- PCC tags (raw).
data PCCTag = PCCDataTag T.Text T.Text
            | PCCBodyTag Lookup
            | PCCComment T.Text deriving Show

type PCCTags = [PCCTag]

-- ! is negation, which really should be separated out in its own
-- parser. there might be other operators, too. but for now...
allCapsAndExclamation :: Parser T.Text
allCapsAndExclamation = takeWhile $ inClass "A-Z!"

parseComment :: Parser PCCTag
parseComment = do
  c <- commentedLine
  return $ PCCComment c

parseDataTag :: Parser PCCTag
parseDataTag = do
  k <- allCapsAndExclamation
  v <- ":" .*> restOfLine
  return $ PCCDataTag k v

linkLocation :: Char -> Location
linkLocation '@' = Data
linkLocation '&' = Vendor
linkLocation _ = Any

parseBodyTag :: Parser PCCTag
parseBodyTag = do
  _ <- "PCC:"
  l <- satisfy $ inClass "@&*"
  v <- restOfLine
  return $ PCCBodyTag Lookup { location = linkLocation l,
                               filename = v }

parsePCCLine :: Parser PCCTags
parsePCCLine = many $ (parseBodyTag <|>
                       parseDataTag <|>
                       parseComment) <* many endOfLine

parsePCC :: FilePath -> IO PCCTags
parsePCC pccName = do
  contents <- readContents pccName
  print $ "** parsing PCC: " ++ pccName
  let result = parse parsePCCLine contents in
    return $ parseResult pccName result
