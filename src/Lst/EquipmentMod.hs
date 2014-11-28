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
                            | Visible VisibleTag
                            | Cost Formula
                            | SpecialProperty Property
                            | Type [String]
                              deriving Show

data VisibleTag = No
                | Yes
                | Qualify
                  deriving (Show, Eq)

parseVisible :: PParser VisibleTag
parseVisible = tag "VISIBLE" *> parseVisibleTag where
  parseVisibleTag = No <$ labeled "NO"
                <|> Yes <$ labeled "YES"
                <|> Qualify <$ labeled "QUALIFY"

data Property = Property { specialPropertyText :: String
                         , specialPropertyVariables :: [Formula]
                         , specialPropertyRestrictions :: [RestrictionTag] }
              deriving (Show, Eq)

-- parseSpecialProperty :: PParser Property
-- parseSpecialProperty = do
--   _ <- tag "SPROP"
--   specialPropertyText <- many1 $ noneOf "\t\r\n|"
--   -- don't bother trying to gauge the number of '%'s. some SPROP text
--   -- is unescaped so we just have to blindly read in the variables.
--   specialPropertyVariables <- many parsePossibleFormula
--   specialPropertyRestrictions <- option [] parseAdditionalRestrictions
--   return Property { .. } where
--     parsePossibleFormula = notAllowed *> char '|' *> parseFormula
--     notAllowed = notFollowedBy (string "|PRE") *> notFollowedBy (string "|!PRE")

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser [String]
parseType = tag "TYPE" >> parseString `sepBy` char '.'

parseCost :: PParser Formula
parseCost = tag "COST" *> parseFormula

parseEquipmentModTag :: PParser EquipmentModDefinition
parseEquipmentModTag = Description <$> parseDescription
                   <|> Type <$> parseType
                   <|> Cost <$> parseCost
                   <|> Visible <$> parseVisible
                   -- <|> SpecialProperty <$> parseSpecialProperty

instance LSTObject EquipmentModDefinition where
  parseSpecificTags = parseEquipmentModTag
