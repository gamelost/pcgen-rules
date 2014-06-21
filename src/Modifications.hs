{-# LANGUAGE OverloadedStrings #-}

module Modifications where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Monad(liftM)
import Common

data Operation = Add | Modify | Forget deriving Show

data Modification a = Modification { operation :: Operation
                                   , record :: a } deriving Show

parseStartString :: Parser T.Text
parseStartString = takeWhile1 $ inClass "-A-Za-z0-9 /'()" -- no punctuation

parseModSuffix :: Parser T.Text
parseModSuffix = do
  what <- parseStartString
  _ <- string ".MOD"
  return what

parseForgetSuffix :: Parser T.Text
parseForgetSuffix = do
  what <- parseStartString
  _ <- string ".FORGET"
  return what

parseMod :: Parser (T.Text, Operation)
parseMod = liftM (\x -> (x, Modify)) parseModSuffix

parseForget :: Parser (T.Text, Operation)
parseForget = liftM (\x -> (x, Forget)) parseForgetSuffix

parseAdd :: Parser (T.Text, Operation)
parseAdd = liftM (\x -> (x, Add)) parseString
