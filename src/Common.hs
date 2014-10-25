{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Monad (liftM)
import Control.Monad.State (State, runState)
import Control.Applicative ((<*), (*>), (<|>))
import Text.Parsec.Text()
import Text.Parsec.Char (char, string, anyChar, satisfy, spaces, tab, newline, upper, digit)
import Text.Parsec.Combinator (many1, sepBy, manyTill, option, optionMaybe, skipMany1, eof)
import Text.Parsec.Prim (try, skipMany, ParsecT, (<?>), runParserT, lookAhead)
import Debug.Trace(trace)
import Prelude

type Variables = M.Map String Int

type PParser a = ParsecT T.Text () (State Variables) a

warning :: String -> a -> a
warning x = trace $ "Warning: " ++ x

-- conversion from attoparsec -> parsec
inClass :: String -> Char -> Bool
inClass "" = const False
inClass (a:'-':b:xs) = \c -> (c >= a && c <= b) || f c where f = inClass xs
inClass (x:xs) = \c -> c == x || f c where f = inClass xs

tryStrings :: [String] -> [PParser String]
tryStrings = map $ try . string

tryOption :: PParser a -> PParser (Maybe a)
tryOption = optionMaybe . try

parseWord :: PParser String
parseWord = many1 $ satisfy $ inClass "-A-Za-z"

parseString :: PParser String
parseString = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~" -- do not put in '=' or '|'

parseStringNoCommas :: PParser String
parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~"

parseStringNoCommasBrackets :: PParser String
parseStringNoCommasBrackets = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()~"

allCaps :: PParser String
allCaps = many1 upper

manyNumbers :: PParser String
manyNumbers = many1 digit

tabs :: PParser ()
tabs = skipMany tab

tabs1 :: PParser ()
tabs1 = skipMany1 tab

labeled :: String -> PParser String
labeled = try . string

tag :: String -> PParser String
tag t = labeled $ t ++ ":"

textToInt :: String -> Int
textToInt t = read t :: Int

textToFloat :: String -> Float
textToFloat t = read t :: Float

-- accomodate crlf line terminators.
crlf :: PParser Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"

eol :: PParser Char
eol = newline <|> crlf <?> "eol"

restOfLine :: PParser String
restOfLine = manyTill anyChar eol

restOfTag :: PParser String
restOfTag = manyTill anyChar . lookAhead . satisfy $ inClass "\n\r\t"

parseTabs :: PParser [String]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: PParser String
parseCommentLine = char '#' >> option "" (try $ spaces >> restOfLine)

parseWordAndNumber :: PParser String
parseWordAndNumber = many1 $ satisfy $ inClass "-A-Za-z0-9"

parseTill :: Char -> PParser String
parseTill c = manyTill anyChar $ char c

yesOrNo :: PParser Bool
yesOrNo = liftM (== "YES") allCaps

parseResult :: Show a => PParser a -> FilePath -> T.Text -> a
parseResult parser filename contents =
  let doall = parser <* eof in
  let result = runParserT doall () filename contents in
  case runState result M.empty of
    (Left err, vars) -> error $
      show err ++ "\nvars:\n" ++ show vars
    (Right success, vars) ->
      let _ = trace $ "variable dump: " ++ show vars in
      success

readContents :: FilePath -> IO T.Text
readContents filename = B.readFile filename >>= spit where
  spit x = return $ decodeUtf8With lenientDecode x
