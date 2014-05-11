{-# LANGUAGE OverloadedStrings #-}

module Pcc where

import Prelude hiding (takeWhile, readFile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Text.IO(readFile)
import Control.Applicative
import Fs

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
allCaps :: Parser T.Text
allCaps = takeWhile $ inClass "A-Z!"

restOfLine :: Parser T.Text
restOfLine =
  takeTill (\c -> c == '\n' || c == '\r')

parseComment :: Parser PCCTag
parseComment = do
  c <- "#" .*> restOfLine
  return $ PCCComment c

parseDataTag :: Parser PCCTag
parseDataTag = do
  k <- allCaps
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

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser is incorrect here, we want instant failure.
parsePCCResult :: FilePath -> IResult T.Text t -> t
parsePCCResult pccFilename result =
  case result of
    Done left success | left == T.empty ->
      success
    Partial c ->
        -- just give the continuation an empty result and try again
        parsePCCResult pccFilename $ c T.empty
    Fail _ _ err ->
      error $ "failed to parse " ++ pccFilename ++ " with error " ++ err
    _ ->
      error $ "failed to parse " ++ pccFilename

parsePCC :: FilePath -> IO PCCTags
parsePCC pccName = do
  contents <- readFile pccName
  print $ "** parsing " ++ pccName
  let result = parse parsePCCLine contents in
    return $ parsePCCResult pccName result
