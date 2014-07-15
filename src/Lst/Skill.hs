{-# LANGUAGE OverloadedStrings #-}

module Lst.Skill where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Monad(liftM)
import Control.Applicative
import Restrictions(Restriction, parseRestriction)
import Modifications
import Lst.GlobalTags
import Common
import Bonus

data ArmorCheck = Double
                | Proficient
                | NonProficient
                | Weight
                | Yes
                | No
                  deriving Show

data Visible = Always
             | Display
             | Export
               deriving Show

data Class = AllClasses
           | Subset [String]
             deriving Show

data SkillDefinition = Name String
                     | Type [String]
                     | ArmorClassCheck ArmorCheck
                     | Classes Class
                     | Exclusive Bool
                     | UniqueKey String
                     | Visibility (Visible, Bool)
                     -- shared tags
                     | Global GlobalTag
                     | SkillBonus Bonus
                     | Restricted Restriction
                       deriving Show

type SkillTag = Parser SkillDefinition

parseExclusive :: SkillTag
parseExclusive = Exclusive <$> (tag "EXCLUSIVE" >> yesOrNo)

parseUniqueKey :: SkillTag
parseUniqueKey  = UniqueKey <$> (tag "KEY" >> parseString)

parseType :: SkillTag
parseType = Type <$> (tag "TYPE" >> parseString `sepBy` char '.')

parseClasses :: SkillTag
parseClasses = Classes <$> (tag "CLASSES" >> parseClass) where
  parseClass :: Parser Class
  parseClass = (string "ALL" >> return AllClasses) <|>
               (Subset <$> (parseString `sepBy` char '|'))

parseArmorCheck :: SkillTag
parseArmorCheck = do
  a <- tag "ACHECK" >> liftM matchArmorCheck allCaps
  return $ ArmorClassCheck a where
    matchArmorCheck :: String -> ArmorCheck
    matchArmorCheck "DOUBLE" = Double
    matchArmorCheck "PROFICIENT" = Proficient
    matchArmorCheck "NONPROF" = NonProficient
    matchArmorCheck "WEIGHT" = Weight
    matchArmorCheck "YES" = Yes
    matchArmorCheck _ = No

parseVisibility :: SkillTag
parseVisibility = do
  v <- tag "VISIBLE" >> liftM matchVisibility allCaps
  ro <- option False (string "|READONLY" >> return True)
  return $ Visibility (v, ro) where
    matchVisibility :: String -> Visible
    matchVisibility "ALWAYS" = Always
    matchVisibility "YES" = Always
    matchVisibility "GUI" = Display
    matchVisibility "DISPLAY" = Display
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = Export
    matchVisibility _ = Always

parseSkillTag :: SkillTag
parseSkillTag = parseArmorCheck
            <|> parseType
            <|> parseClasses
            <|> parseExclusive
            <|> parseVisibility
            <|> parseUniqueKey
            <|> Global <$> parseGlobalTags
            <|> SkillBonus <$> parseBonus
            <|> Restricted <$> parseRestriction

parseSkillDefinition :: String -> Parser [SkillDefinition]
parseSkillDefinition name = do
  skillTags <- parseSkillTag `sepBy` tabs
  return $ skillTags ++ [Name name]

instance LSTObject SkillDefinition where
  parseLine = parseSkillDefinition
