{-# LANGUAGE OverloadedStrings #-}

module Util where

import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Attoparsec.Text

allCaps :: Parser T.Text
allCaps = takeWhile $ inClass "A-Z"

restOfLine :: Parser T.Text
restOfLine = takeTill (\c -> c == '\n' || c == '\r')

commentedLine :: Parser T.Text
commentedLine = "#" .*> restOfLine

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser is incorrect here, we want instant failure.
parseResult :: FilePath -> IResult T.Text t -> t
parseResult filename result =
  case result of
    Done left success | left == T.empty ->
      success
    Partial c ->
        -- just give the continuation an empty result and try again
        parseResult filename $ c T.empty
    Fail _ _ err ->
      error $ "failed to parse " ++ filename ++ " with error " ++ err
    _ ->
      error $ "failed to parse " ++ filename

-- Data.Text.IO.readFile does not do the right thing, sigh. Instead,
-- read in the contents as a bytestring and then attempt an utf8
-- decoding.
readContents :: FilePath -> IO T.Text
readContents filename = do
  f <- B.readFile filename
  return $ decodeUtf8 f
