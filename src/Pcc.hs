{-# LANGUAGE OverloadedStrings #-}

module Pcc where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Fs
import Common

-- store the path of a given reference file.
data Lookup = Lookup { location :: Location
                     , filename :: T.Text } deriving Show

-- PCC tags (raw).
data PCCTag = PCCDataTag T.Text T.Text
            | PCCBodyTag Lookup
            | PCCComment T.Text
            | PCCInvert PCCTag
              deriving Show

type PCCTags = [PCCTag]

parseComment :: Parser PCCTag
parseComment = liftM PCCComment parseCommentLine

parseDataTag :: Parser PCCTag
parseDataTag = do
  k <- allCaps
  _ <- char ':'
  v <- restOfLine
  return $ PCCDataTag k v

linkLocation :: Char -> Location
linkLocation '@' = Data
linkLocation '&' = Vendor
linkLocation _ = Any

parseBodyTag :: Parser PCCTag
parseBodyTag = do
  _ <- string "PCC:"
  l <- satisfy $ inClass "@&*"
  v <- restOfLine
  return $ PCCBodyTag Lookup { location = linkLocation l,
                               filename = v }

parseInversion :: Parser PCCTag
parseInversion = PCCInvert <$> (char '!' >> parseDataTag)

parsePCCLine :: Parser PCCTags
parsePCCLine = many $ parseLine <* many endOfLine where
  parseLine = parseBodyTag
          <|> parseDataTag
          <|> parseInversion
          <|> parseComment

parsePCC :: FilePath -> IO PCCTags
parsePCC pccName = do
  contents <- readContents pccName
  --print $ "** parsing PCC: " ++ pccName
  let result = parse parsePCCLine contents in
    return $ parseResult pccName result
