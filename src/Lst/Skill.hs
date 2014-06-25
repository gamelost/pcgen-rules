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

data Visibility = Always
                | Display
                | GUI
                | Export
                | CSheet
                deriving Show

data SkillTag a = SkillSubset String a

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

-- TODO
-- add url link
parseExclusive :: Parser Bool
parseExclusive = tag "EXCLUSIVE" >> yesOrNo

parseUseUntrained :: Parser Bool
parseUseUntrained = tag "USEUNTRAINED" >> yesOrNo

parseKeyStat :: Parser T.Text
parseKeyStat = tag "KEYSTAT" >> parseString

parseSourcePage :: Parser T.Text
parseSourcePage = tag "SOURCEPAGE" >> parseString

parseACheck :: Parser ACheck
parseACheck = tag "ACHECK" >> liftM matchACheck allCaps where
  matchACheck :: T.Text -> ACheck
  matchACheck "DOUBLE" = Double
  matchACheck "PROFICIENT" = Proficient
  matchACheck "NONPROF" = NonProficient
  matchACheck "WEIGHT" = Weight
  matchACheck "YES" = Yes
  matchACheck _ = No

parseVisibility :: Parser (Visibility, Bool)
parseVisibility = do
  visible <- tag "VISIBLE" >> liftM matchVisibility allCaps
  readonly <- option False (string "|READONLY" >> return True)
  return (visible, readonly) where
    -- TODO fix, there are fewer actual values than this
    matchVisibility :: T.Text -> Visibility
    matchVisibility "ALWAYS" = Always
    matchVisibility "DISPLAY" = Display
    matchVisibility "GUI" = GUI
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = CSheet
    matchVisibility _ = Always

addSkill :: SkillTag a -> SkillDefinition -> SkillDefinition
addSkill tag result@SkillDefinition =
  result { tag = result }

parseSkillTag :: Parser (SkillTag a)
parseSkillTag = liftM (SkillSubset "keyStat") parseKeyStat <|>
                liftM (SkillSubset "useUntrained") parseUseUntrained <|>
                liftM (SkillSubset "armorClassCheck") parseACheck <|>
                liftM (SkillSubset "exclusive") parseExclusive <|>
                liftM (SkillSubset "restriction") parseRestriction <|>
                liftM (SkillSubset "readonly") parseVisibility

parseSkillDefinition :: T.Text -> Parser SkillDefinition
parseSkillDefinition p = do
  (languageName, op) <- p <* tabs
  skillTags <- choice parseSkillTag `sepBy` tabs
  return foldr addSkill defaultSkill skillTags
