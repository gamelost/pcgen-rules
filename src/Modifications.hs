{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Modifications where

import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Combinator (eof)
import Data.Maybe (fromJust)
import ClassyPrelude

import Restrictions (parseRestriction, RestrictionTag)
import Lst.GlobalTags (parseGlobal, GlobalTag)
import Bonus (parseBonus, BonusTag)
import Clear (parseClear, ClearTag)

import Common

data Operation = Add | Copy | Modify | Forget deriving Show

data LSTData a = Name String
               | Bonus BonusTag
               | Clear ClearTag
               | Global GlobalTag
               | Restriction RestrictionTag
               | Specific a
                 deriving Show

data LSTLine a = LSTLine { operation :: Operation
                         , tags :: [LSTData a] }
                 deriving Show

class LSTObject a where
  parseSpecificTags :: PParser a

  parseLSTLine :: PParser (LSTLine a)
  parseLSTLine = do
    (name, operation) <- parseStart <* tabs
    allTags <- parseAllTags `sepBy` tabs <* ending
    return LSTLine { operation = operation
                   , tags = Name name : allTags } where
      ending = eol <|> ('\0' <$ eof)
      parseAllTags = Clear <$> parseClear -- should be first
                 <|> Specific <$> parseSpecificTags
                 <|> Restriction <$> parseRestriction
                 <|> Bonus <$> parseBonus
                 <|> Global <$> parseGlobal -- should be last

parseStart :: PParser (String, Operation)
parseStart = do
  what <- restOfTag
  return . fromJust $ matchSuffixes what where
    matchSuffixes str = (\x -> (x, Modify)) <$> stripSuffix ".MOD" str
                    <|> (\x -> (x, Forget)) <$> stripSuffix ".FORGET" str
                    <|> (\x -> (x, Copy)) <$> stripSuffix ".COPY" str
                    <|> return (str, Add)
