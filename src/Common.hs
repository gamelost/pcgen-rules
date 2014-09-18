{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude
import qualified Data.ByteString as B
import Data.ByteString.UTF8(toString)
import Control.Monad(liftM)
import Control.Monad.State(State, runState)
import Control.Applicative((<*), (*>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String()
import Text.Parsec.Prim hiding (State)
import Data.List(stripPrefix)
import qualified Data.Map.Strict as M
import Debug.Trace(trace)

type Variables = M.Map String Int
type PParser a = ParsecT String () (State Variables) a

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

-- accomodate crlf line terminators.
crlf :: PParser Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"

eol :: PParser Char
eol = newline <|> crlf <?> "eol"

restOfLine :: PParser String
restOfLine = manyTill anyChar eol

restOfTag :: PParser String
restOfTag = manyTill anyChar $ satisfy $ inClass "\n\r\t"

parseTabs :: PParser [String]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: PParser String
parseCommentLine = char '#' >> option "" (try $ spaces >> restOfLine)

parseWordAndNumber :: PParser String
parseWordAndNumber = many1 $ satisfy $ inClass "-A-Za-z0-9"

yesOrNo :: PParser Bool
yesOrNo = liftM (== "YES") allCaps

stripSuffix :: String -> String -> Maybe String
stripSuffix sfx rest = case stripPrefix (reverse sfx) (reverse rest) of
  Just ys -> Just (reverse ys)
  Nothing -> Nothing

parseResult :: Show a => PParser a -> FilePath -> String -> a
parseResult parser filename contents =
  let doall = parser <* eof in
  let result = runParserT doall () filename contents in
  case runState result M.empty of
    (Left err, vars) -> error $
      show err ++ "\nvars:\n" ++ show vars
    (Right success, vars) ->
      let _ = trace $ "variable dump: " ++ show vars in
      success

readContents :: FilePath -> IO String
readContents filename = B.readFile filename >>= spit where
  spit x = return $ toString x
