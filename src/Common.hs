{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)
import Control.Monad(liftM)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative hiding (many)
import qualified Text.Show.Pretty as Pretty
import Debug.Trace(trace)

-- conversion from attoparsec -> parsec
inClass :: String -> Char -> Bool
inClass "" = const False
inClass (a:'-':b:xs) = \c -> (c >= a && c <= b) || f c where f = inClass xs
inClass (x:xs) = \c -> c == x || f c where f = inClass xs

parseWord :: Parser String
parseWord = many1 $ satisfy $ inClass "-A-Za-z"

parseString :: Parser String
parseString = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~" -- do not put in '=' or '|'

allCaps :: Parser String
allCaps = many1 $ satisfy $ inClass "A-Z"

manyNumbers :: Parser String
manyNumbers = many1 $ satisfy $ inClass "0-9"

tabs :: Parser ()
tabs = skipMany1 $ char '\t'

tag :: String -> Parser ()
tag t = string t >> char ':' >> empty

textToInt :: String -> Int
textToInt t = read t :: Int

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||>

restOfLine :: Parser String
restOfLine = manyTill anyChar $ satisfy ((== '\n') <||> (== '\r'))

restOfTag :: Parser String
restOfTag = manyTill anyChar $ satisfy ((== '\n') <||> (== '\r') <||> (== '\t'))

parseTabs :: Parser [String]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: Parser String
parseCommentLine = char '#' >> spaces >> restOfLine where

parseWordAndNumber :: Parser String
parseWordAndNumber = many1 $ satisfy $ inClass "-A-Za-z0-9"

yesOrNo :: Parser Bool
yesOrNo = liftM (== "YES") allCaps

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser does not consume everything, we want instant
-- failure.
parseResult :: Show t => FilePath -> IResult String t -> t
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
