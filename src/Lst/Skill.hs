{-# LANGUAGE OverloadedStrings #-}

module Lst.Skill where

import qualified Data.Text as T
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative
import Restrictions
import Modifications
import Common

data ACheck = Double
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

data SkillDefinition = Name T.Text
                     | ArmorClassCheck ACheck
                     | Classes [T.Text]
                     | Exclusive Bool
                     | KeyStat T.Text
                     | UseUntrained Bool
                     | SourcePage T.Text
                     | Visibility (Visible, Bool)
                     | Restricted Restriction
                       deriving Show

type SkillTag = Parser SkillDefinition

parseExclusive :: SkillTag
parseExclusive = Exclusive <$> (tag "EXCLUSIVE" >> yesOrNo)

parseUseUntrained :: SkillTag
parseUseUntrained = UseUntrained <$> (tag "USEUNTRAINED" >> yesOrNo)

parseKeyStat :: SkillTag
parseKeyStat  = KeyStat <$> (tag "KEYSTAT" >> parseString)

parseSourcePage :: SkillTag
parseSourcePage  = SourcePage <$> (tag "SOURCEPAGE" >> parseString)

parseACheck :: SkillTag
parseACheck = do
  a <- tag "ACHECK" >> liftM matchACheck allCaps
  return $ ArmorClassCheck a where
    matchACheck :: T.Text -> ACheck
    matchACheck "DOUBLE" = Double
    matchACheck "PROFICIENT" = Proficient
    matchACheck "NONPROF" = NonProficient
    matchACheck "WEIGHT" = Weight
    matchACheck "YES" = Yes
    matchACheck _ = No

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
parseSkillTag = parseKeyStat <|>
                parseUseUntrained <|>
                parseACheck <|>
                parseExclusive <|>
                parseVisibility <|>
                Restricted <$> parseRestriction

parseSkillDefinition :: T.Text -> Parser [SkillDefinition]
parseSkillDefinition name = do
  skillTags <- parseSkillTag `sepBy` tabs
  return $ skillTags ++ [Name name]

instance LSTObject SkillDefinition where
  parseLine = parseSkillDefinition