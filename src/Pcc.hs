{-# LANGUAGE OverloadedStrings #-}

module Pcc where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import Control.Applicative hiding (many)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim hiding ((<|>))
import Fs
import Common

-- store the path of a given reference file.
data Lookup = Lookup { location :: Location
                     , filename :: String } deriving Show

-- PCC tags (raw).
data PCCTag = PCCDataTag String String
            | PCCBodyTag Lookup
            | PCCComment String
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
  _ <- labeled "PCC:"
  l <- oneOf "@&*"
  v <- restOfLine
  return $ PCCBodyTag Lookup { location = linkLocation l,
                               filename = v }

parseInversion :: Parser PCCTag
parseInversion = PCCInvert <$> (char '!' >> parseDataTag)

parsePCCLine :: Parser PCCTags
parsePCCLine = many $ parseLine <* many newline where
  parseLine = parseBodyTag
          <|> parseDataTag
          <|> parseInversion
          <|> parseComment

parsePCC :: FilePath -> IO PCCTags
parsePCC pccName = do
  contents <- readContents pccName
  --print $ "** parsing PCC: " ++ pccName
  return $ parseResult parsePCCLine pccName contents
