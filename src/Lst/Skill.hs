{-# LANGUAGE OverloadedStrings #-}

module Lst.Skill where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Control.Monad(liftM)
import Control.Applicative
import Restrictions(Restriction, parseRestriction)
import Modifications
import Lst.GlobalTags
import Common
import Clear(parseClear, ClearTag(..))
import Bonus(parseBonus, Bonus)

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
                     | SkillClear ClearTag
                     | Global GlobalTag
                     | SkillBonus Bonus
                     | Restricted Restriction
                       deriving Show

type SkillTag = PParser SkillDefinition

parseExclusive :: SkillTag
parseExclusive = Exclusive <$> (tag "EXCLUSIVE" >> yesOrNo)

parseUniqueKey :: SkillTag
parseUniqueKey  = UniqueKey <$> (tag "KEY" >> restOfTag)

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
parseSkillTag = SkillClear <$> parseClear
            <|> parseArmorCheck
            <|> parseType
            <|> parseClasses
            <|> parseExclusive
            <|> parseVisibility
            <|> parseUniqueKey
            <|> SkillBonus <$> parseBonus
            <|> Restricted <$> parseRestriction
            <|> Global <$> parseGlobalTags

parseSkillDefinition :: String -> PParser [SkillDefinition]
parseSkillDefinition name = do
  skillTags <- parseSkillTag `sepBy` tabs
  return $ skillTags ++ [Name name]

instance LSTObject SkillDefinition where
  parseLine = parseSkillDefinition
