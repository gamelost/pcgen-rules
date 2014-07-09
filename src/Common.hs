{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)
import Control.Monad(liftM)
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Text.Show.Pretty as Pretty
import Debug.Trace(trace)

parseWord :: Parser T.Text
parseWord = takeWhile1 $ inClass "-A-Za-z"

parseString :: Parser T.Text
parseString = takeWhile1 $ inClass "-A-Za-z0-9_ &+,./:?!#'()[]~" -- do not put in '=' or '|'

allCaps :: Parser T.Text
allCaps = takeWhile1 $ inClass "A-Z"

manyNumbers :: Parser T.Text
manyNumbers = takeWhile1 $ inClass "0-9"

tabs :: Parser ()
tabs = skipWhile (== '\t')

tag :: String -> Parser Char
tag t = string (T.pack t) >> char ':'

textToInt :: T.Text -> Int
textToInt t = read (T.unpack t) :: Int

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||>

restOfLine :: Parser T.Text
restOfLine = takeTill ((== '\n') <||> (== '\r'))

restOfTag :: Parser T.Text
restOfTag = takeTill ((== '\n') <||> (== '\r') <||> (== '\t'))

parseTabs :: Parser [T.Text]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: Parser T.Text
parseCommentLine = char '#' >> skipWhile (== ' ') >> restOfLine

parseWordAndNumber :: Parser T.Text
parseWordAndNumber = takeWhile1 $ inClass "-A-Za-z0-9"

yesOrNo :: Parser Bool
yesOrNo = liftM (== "YES") allCaps

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser does not consume everything, we want instant
-- failure.
parseResult :: Show t => FilePath -> IResult T.Text t -> t
parseResult filename result =
  case result of
    Done left success | left == T.empty ->
      success
    Done left r ->
      let nextTag = dropWhile (== '\t') (T.unpack left) in
      let filterTag x = x /= '\n' && x /= '\r' in
      let unparsedTag = Prelude.takeWhile filterTag nextTag in
      trace (Pretty.ppShow r)
      error $ "failed to parse " ++ filename ++
                " with remaining input: '" ++ unparsedTag ++ "'"
    Partial c ->
      -- just give the continuation an empty result and try again
      parseResult filename $ c T.empty
    Fail _ _ err ->
      error $ "failed to parse " ++ filename ++ " with error " ++ err

-- Data.Text.IO.readFile does not do the right thing, sigh. Instead,
-- read in the contents as a bytestring and then attempt an utf8
-- decoding. We need to do this in order to parse certain LST files.
readContents :: FilePath -> IO T.Text
readContents filename = do
  f <- B.readFile filename
  return $ decodeUtf8With lenientDecode f
