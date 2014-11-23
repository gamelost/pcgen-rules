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
                       deriving Show

type SkillTag = PParser SkillDefinition

parseExclusive :: SkillTag
parseExclusive = Exclusive <$> (tag "EXCLUSIVE" >> yesOrNo)

parseType :: SkillTag
parseType = Type <$> (tag "TYPE" >> parseString `sepBy` char '.')

parseClasses :: SkillTag
parseClasses = Classes <$> (tag "CLASSES" >> parseClass) where
  parseClass :: PParser Class
  parseClass = try (AllClasses <$ labeled "ALL")
           <|> (Subset <$> (parseString `sepBy` char '|'))

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
  ro <- option False (True <$ labeled "|READONLY")
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

instance LSTObject SkillDefinition where
  parseSpecificTags = parseSkillTag
