{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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

data Visibility = Always
                | Display
                | Export
                deriving Show

data SkillDefinition = SkillDefinition { name :: T.Text
                                       , armorClassCheck :: ACheck
                                       , classes :: [T.Text]
                                       , exclusive :: Bool
                                       , keyStat :: Maybe T.Text
                                       , useUntrained :: Bool
                                       , sourcePage :: Maybe T.Text
                                       , visibility :: Visibility
                                       , readOnly :: Bool
                                       , restriction :: Maybe Restriction
                                       } deriving Show

defaultSkill = SkillDefinition { name = "undefined"
                               , armorClassCheck = No
                               , classes = []
                               , exclusive = False
                               , keyStat = Nothing
                               , useUntrained = False
                               , sourcePage = Nothing
                               , visibility = Always
                               , readOnly = False
                               , restriction = Nothing }

-- NB this type is needed for parsing of unordered tags

type SkillTag = Parser (SkillDefinition -> SkillDefinition)

parseExclusive :: SkillTag
parseExclusive = do
  e <- tag "EXCLUSIVE" >> yesOrNo
  return (\result@SkillDefinition{ .. } -> result { exclusive = e })

parseUseUntrained :: SkillTag
parseUseUntrained = do
  u <- tag "USEUNTRAINED" >> yesOrNo
  return (\result@SkillDefinition{ .. } -> result { useUntrained = u })

parseKeyStat :: SkillTag
parseKeyStat  = do
  k <- tag "KEYSTAT" >> parseString
  return (\result@SkillDefinition{ .. } -> result { keyStat = Just k })

parseSourcePage :: SkillTag
parseSourcePage  = do
  s <- tag "SOURCEPAGE" >> parseString
  return (\result@SkillDefinition{ .. } -> result { sourcePage = Just s })

parseACheck :: SkillTag
parseACheck = do
  a <- tag "ACHECK" >> liftM matchACheck allCaps
  return (\result@SkillDefinition{ .. } -> result { armorClassCheck = a }) where
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
  return (\result@SkillDefinition{ .. } -> result { visibility = v, readOnly = ro }) where
    matchVisibility :: T.Text -> Visibility
    matchVisibility "ALWAYS" = Always
    matchVisibility "YES" = Always
    matchVisibility "GUI" = Display
    matchVisibility "DISPLAY" = Display
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = Export
    matchVisibility _ = Always

-- XXX add classes

parseSkillTag :: SkillTag
parseSkillTag = parseKeyStat <|>
                parseUseUntrained <|>
                parseACheck <|>
                parseExclusive <|>
                -- parseRestriction <|>
                parseVisibility

applySkillName :: T.Text -> SkillDefinition -> SkillDefinition
applySkillName skillName result@SkillDefinition{ .. } = result { name = skillName }

parseSkillDefinition :: T.Text -> Parser SkillDefinition
parseSkillDefinition name = do
  skillTags <- parseSkillTag `sepBy` tabs
  let updatedSkillTags = applySkillName name defaultSkill
  return $ foldr id updatedSkillTags skillTags

instance LSTObject SkillDefinition where
  parseLine = parseSkillDefinition
