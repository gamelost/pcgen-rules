{-# LANGUAGE OverloadedStrings #-}

module Lst.Spell where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy, option)
import ClassyPrelude

import Restrictions (RestrictionTag, parseRestriction)
import Modifications
import JEPFormula hiding (Add)
import Common

data SpellDefinition = Description String
                     | Classes Class
                     | Range String
                     | SaveInfo String
                     | CastTime String
                     | TargetArea String
                     | Duration String
                     | Schools [String]
                       deriving Show

data ClassType = ClassName String
               | ClassType String
               | AllClasses
                 deriving (Show, Eq)

data Class = Class { classTypes :: [([ClassType], Int)]
                   , classRestrictions :: Maybe RestrictionTag }
           deriving (Show, Eq)

parseClasses :: PParser Class
parseClasses = do
  _ <- tag "CLASSES"
  classTypes <- parseClassTypes `sepBy` char '|'
  -- restrictions are denoted by [...]
  classRestrictions <- tryOption (char '[' >> parseRestriction <* char ']')
  return Class { .. } where
    parseClassTypes = do
      types <- parseClassType `sepBy` char ','
      _ <- char '='
      number <- parseInteger
      return (types, number)
    parseClassType = AllClasses <$ labeled "ALL"
                 <|> (labeled "TYPE." *> (ClassType <$> parseStringNoCommas))
                 <|> (ClassName <$> parseStringNoCommas)

parseSchool :: PParser [String]
parseSchool = tag "SCHOOL" *> parseString `sepBy` char '|'

-- TODO: parse formulas in parentheses
parseDuration :: PParser String
parseDuration = tag "DURATION" *> restOfTag

-- TODO: parse formulas in parentheses
parseTargetArea :: PParser String
parseTargetArea = tag "TARGETAREA" *> restOfTag

parseCastTime :: PParser String
parseCastTime = tag "CASTTIME" *> restOfTag

parseSaveInfo :: PParser String
parseSaveInfo = tag "SAVEINFO" *> restOfTag

parseRange :: PParser String
parseRange = tag "RANGE" *> restOfTag

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseSpellTag :: PParser SpellDefinition
parseSpellTag = Classes <$> parseClasses
            <|> Description <$> parseDescription
            <|> Range <$> parseRange
            <|> SaveInfo <$> parseSaveInfo
            <|> CastTime <$> parseCastTime
            <|> TargetArea <$> parseTargetArea
            <|> Duration <$> parseDuration
            <|> Schools <$> parseSchool

   -- 1356 SOURCELINK with contents
   -- 1585 DOMAINS with contents
   -- 2549 ITEM with contents
   -- 2657 SUBSCHOOL with contents
   -- 2965 DESCRIPTOR with contents
   -- 8066 TYPE with contents
   -- 8117 SPELLRES with contents
   -- 8297 COMPS with contents

instance LSTObject SpellDefinition where
  parseSpecificTags = parseSpellTag
