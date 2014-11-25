{-# LANGUAGE OverloadedStrings #-}

module Lst.Skill where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy, option)
import Text.Parsec.Prim (try)
import ClassyPrelude hiding (try)

import Modifications
import Common

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

data SkillDefinition = Type [String]
                     | ArmorClassCheck ArmorCheck
                     | Classes Class
                     | Exclusive Bool
                     | Visibility (Visible, Bool)
                     | Situation String
                       deriving Show

parseExclusive :: PParser Bool
parseExclusive = tag "EXCLUSIVE" >> yesOrNo

parseType :: PParser [String]
parseType = tag "TYPE" >> parseString `sepBy` char '.'

parseSituation :: PParser String
parseSituation = tag "SITUATION" *> restOfTag

parseClasses :: PParser Class
parseClasses = tag "CLASSES" >> parseClass where
  parseClass :: PParser Class
  parseClass = try (AllClasses <$ labeled "ALL")
           <|> (Subset <$> (parseString `sepBy` char '|'))

parseArmorCheck :: PParser ArmorCheck
parseArmorCheck = tag "ACHECK" >> liftM matchArmorCheck allCaps where
    matchArmorCheck :: String -> ArmorCheck
    matchArmorCheck "DOUBLE" = Double
    matchArmorCheck "PROFICIENT" = Proficient
    matchArmorCheck "NONPROF" = NonProficient
    matchArmorCheck "WEIGHT" = Weight
    matchArmorCheck "YES" = Yes
    matchArmorCheck _ = No

parseVisibility :: PParser (Visible, Bool)
parseVisibility = do
  v <- tag "VISIBLE" >> liftM matchVisibility allCaps
  ro <- option False (True <$ labeled "|READONLY")
  return (v, ro) where
    matchVisibility :: String -> Visible
    matchVisibility "ALWAYS" = Always
    matchVisibility "YES" = Always
    matchVisibility "GUI" = Display
    matchVisibility "DISPLAY" = Display
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = Export
    matchVisibility _ = Always

parseSkillTag :: PParser SkillDefinition
parseSkillTag = ArmorClassCheck <$> parseArmorCheck
            <|> Type <$> parseType
            <|> Classes <$> parseClasses
            <|> Exclusive <$> parseExclusive
            <|> Visibility <$> parseVisibility
            <|> Situation <$> parseSituation

instance LSTObject SkillDefinition where
  parseSpecificTags = parseSkillTag
