{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative

parseWord :: Parser T.Text
parseWord = takeWhile1 $ inClass "-A-Za-z"

parseString :: Parser T.Text
parseString = takeWhile1 $ inClass "-A-Za-z0-9_ ,./:?'()" -- do not put in '='

parseUrl :: Parser T.Text
parseUrl = takeWhile1 $ inClass "-A-Za-z0-9_ =%&,./:?'()"

restOfLine :: Parser T.Text
restOfLine = takeTill (\c -> c == '\n' || c == '\r')

-- ! is negation, which really should be separated out in its own
-- parser. there might be other operators, too. but for now...
allCaps :: Parser T.Text
allCaps = takeWhile1 $ inClass "A-Z!"

manyNumbers :: Parser T.Text
manyNumbers = takeWhile1 $ inClass "0-9"

tabs :: Parser ()
tabs = skipWhile (== '\t')

parseCommentLine :: Parser T.Text
parseCommentLine = "#" .*> restOfLine

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser does not consume everything, we want instant
-- failure.
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
-- decoding. We need to do this in order to parse certain LST files.
readContents :: FilePath -> IO T.Text
readContents filename = do
  f <- B.readFile filename
  return $ decodeUtf8With lenientDecode f
