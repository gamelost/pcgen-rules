{-# LANGUAGE OverloadedStrings #-}

module Modifications where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Monad(liftM)
import Common

-- intentionally simple, for now
data Modification = Add | Reset deriving Show

parseModString :: Parser T.Text
parseModString = takeWhile1 $ inClass "-A-Za-z0-9 /'()" -- no punctuation

parseModification :: Parser T.Text
parseModification = do
  what <- parseModString
  _ <- string ".MOD"
  return what

parseStartMod :: Parser (T.Text, Maybe Modification)
parseStartMod = liftM (\x -> (x, Just Add)) parseModification

parseStart :: Parser (T.Text, Maybe Modification)
parseStart = liftM (\x -> (x, Nothing)) parseString
