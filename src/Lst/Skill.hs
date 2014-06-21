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

data Skill = Skill { name :: T.Text
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

defaultSkill = Skill { name = "undefined"
                     , armorClassCheck = No
                     , classes = []
                     , exclusive = False
                     , keyStat = Nothing
                     , useUntrained = False
                     , sourcePage = Nothing
                     , visibility = Always
                     , readOnly = False
                     , restriction = Nothing }

type SkillMod = Modification Skill
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
    matchVisibility :: T.Text -> Visibility
    matchVisibility "ALWAYS" = Always
    matchVisibility "DISPLAY" = Display
    matchVisibility "GUI" = GUI
    matchVisibility "EXPORT" = Export
    matchVisibility "CSHEET" = CSheet
    matchVisibility _ = Always

parseSkillDefinition :: Parser (T.Text, Operation) -> Parser SkillMod
parseSkillDefinition p = do
  (languageName, op) <- p <* tabs
  keystat <- optionMaybe parseKeyStat <* tabs
  untrained <- option False parseUseUntrained <* tabs
  -- classes <- optionMaybe parseKey <* tabs
  acheck <- option No parseACheck <* tabs
  isExclusive <- option False parseExclusive <* tabs
  languageRestriction <- option Nothing parseRestriction <* tabs
  (isVisible, readonly) <- option (Always, True) parseVisibility <* tabs
  page <- optionMaybe parseSourcePage <* tabs
  return Modification { operation = op
                      , record = defaultSkill { name = languageName
                                              , armorClassCheck = acheck
                                              , classes = []
                                              , exclusive = isExclusive
                                              , useUntrained = untrained
                                              , keyStat = keystat
                                              , sourcePage = page
                                              , visibility = isVisible
                                              , readOnly = readonly
                                              , restriction = languageRestriction } }

parseSkillLine :: Parser SkillMod
parseSkillLine = parseSkillDefinition parseMod <|>
                 parseSkillDefinition parseAdd
