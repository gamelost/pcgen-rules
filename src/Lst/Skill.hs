{-# LANGUAGE OverloadedStrings #-}

module Lst.Skill where

import qualified Data.Text as T
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative
import Restrictions(Restriction, parseRestriction)
import Modifications
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
           | Subset [T.Text]
             deriving Show

data SkillDefinition = Name T.Text
                     | Type [T.Text]
                     | ArmorClassCheck ArmorCheck
                     | Classes Class
                     | Exclusive Bool
                     | UniqueKey T.Text
                     | KeyStat T.Text
                     | UseUntrained Bool
                     | SourcePage T.Text
                     | ProductIdentity Bool
                     | TemporaryDescription T.Text
                     | OutputName T.Text
                     | Visibility (Visible, Bool)
                     | SkillBonus Bonus
                     | Restricted Restriction
                       deriving Show

type SkillTag = Parser SkillDefinition

parseProductIdentity :: SkillTag
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

parseExclusive :: SkillTag
parseExclusive = Exclusive <$> (tag "EXCLUSIVE" >> yesOrNo)

parseUseUntrained :: SkillTag
parseUseUntrained = UseUntrained <$> (tag "USEUNTRAINED" >> yesOrNo)

parseOutputName :: SkillTag
parseOutputName = OutputName <$> (tag "OUTPUTNAME" >> parseString)

parseKeyStat :: SkillTag
parseKeyStat  = KeyStat <$> (tag "KEYSTAT" >> parseString)

parseUniqueKey :: SkillTag
parseUniqueKey  = UniqueKey <$> (tag "KEY" >> parseString)

parseSourcePage :: SkillTag
parseSourcePage  = SourcePage <$> (tag "SOURCEPAGE" >> parseString)

parseType :: SkillTag
parseType = Type <$> (tag "TYPE" >> parseString `sepBy` char '.')

parseClasses :: SkillTag
parseClasses = Classes <$> (tag "CLASSES" >> parseClass) where
  parseClass :: Parser Class
  parseClass = (string "ALL" >> return AllClasses) <|>
               (Subset <$> (parseString `sepBy` char '|'))

parseTemporaryDescription :: SkillTag
parseTemporaryDescription = TemporaryDescription <$> (tag "TEMPDESC" >> parseString)

parseArmorCheck :: SkillTag
parseArmorCheck = do
  a <- tag "ACHECK" >> liftM matchArmorCheck allCaps
  return $ ArmorClassCheck a where
    matchArmorCheck :: T.Text -> ArmorCheck
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
    matchVisibility :: T.Text -> Visible
    matchVisibility "ALWAYS" = Always
    matchVisibility "YES" = Always
    matchVisibility "GUI" = Display
    matchVisibility "DISPLAY" = Display
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = Export
    matchVisibility _ = Always

parseSkillTag :: SkillTag
parseSkillTag = parseKeyStat
            <|> parseUseUntrained
            <|> parseArmorCheck
            <|> parseType
            <|> parseClasses
            <|> parseTemporaryDescription
            <|> parseSourcePage
            <|> parseExclusive
            <|> parseVisibility
            <|> parseProductIdentity
            <|> parseOutputName
            <|> parseUniqueKey
            <|> SkillBonus <$> parseBonus
            <|> Restricted <$> parseRestriction

parseSkillDefinition :: T.Text -> Parser [SkillDefinition]
parseSkillDefinition name = do
  skillTags <- parseSkillTag `sepBy` tabs
  return $ skillTags ++ [Name name]

instance LSTObject SkillDefinition where
  parseLine = parseSkillDefinition
