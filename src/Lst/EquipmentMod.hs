{-# LANGUAGE OverloadedStrings #-}

module Lst.EquipmentMod where

import Text.Parsec.Char (char, satisfy, noneOf, string)
import Text.Parsec.Combinator (sepBy, sepBy1, many1, option, optional, notFollowedBy)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import Modifications
import JEPFormula
import Common

data EquipmentModDefinition = Description String
                            | Type [String]
                              deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser [String]
parseType = tag "TYPE" >> parseString `sepBy` char '.'

parseEquipmentModTag :: PParser EquipmentModDefinition
parseEquipmentModTag = Description <$> parseDescription
                   <|> Type <$> parseType

instance LSTObject EquipmentModDefinition where
  parseSpecificTags = parseEquipmentModTag
