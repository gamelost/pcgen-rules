{-# LANGUAGE OverloadedStrings #-}

module Modifications where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Monad(liftM)
import Common

-- intentionally simple, for now
data Modification = Add | Forget | Reset deriving Show

parseStartString :: Parser T.Text
parseStartString = takeWhile1 $ inClass "-A-Za-z0-9 /'()" -- no punctuation

parseModification :: Parser T.Text
parseModification = do
  what <- parseStartString
  _ <- string ".MOD"
  return what

parseForget :: Parser T.Text
parseForget = do
  what <- parseStartString
  _ <- string ".FORGET"
  return what

parseStartMod :: Parser (T.Text, Maybe Modification)
parseStartMod = liftM (\x -> (x, Just Add)) parseModification

parseStartForget :: Parser (T.Text, Maybe Modification)
parseStartForget = liftM (\x -> (x, Just Forget)) parseForget

parseStart :: Parser (T.Text, Maybe Modification)
parseStart = liftM (\x -> (x, Nothing)) parseString
