{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.GlobalTags (GlobalTag, parseGlobalTags) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import Restrictions(Restriction, parseAdditionalRestrictions)
import JEPFormula
import Common

data GlobalTag = KeyStat String
               | UseUntrained Bool
               | SortKey String
               | SourcePage String
               | SourceWeb String
               | ProductIdentity Bool
               | OutputName String
               | AbilityTag Ability
               | Select Formula
               | Define NewVariable
               | AutoLanguageTag AutoLanguage
               | ClassSkill [String]
               | ChooseLanguageTag [ChooseLanguage]
               | ChooseSkillTag [ChooseSkill]
                 deriving (Eq, Show)

parseSortKey :: PParser GlobalTag
parseSortKey = SortKey <$> (tag "SORTKEY" >> parseString)

parseKeyStat :: PParser GlobalTag
parseKeyStat = KeyStat <$> (tag "KEYSTAT" >> parseString)

parseUseUntrained :: PParser GlobalTag
parseUseUntrained = UseUntrained <$> (tag "USEUNTRAINED" >> yesOrNo)

parseSourcePage :: PParser GlobalTag
parseSourcePage  = SourcePage <$> (tag "SOURCEPAGE" >> parseString)

parseProductIdentity :: PParser GlobalTag
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

parseOutputName :: PParser GlobalTag
parseOutputName = OutputName <$> (tag "OUTPUTNAME" >> parseString)

parseSelect :: PParser GlobalTag
parseSelect = Select <$> (tag "SELECT" >> parseFormula)

parseSourceWeb :: PParser GlobalTag
parseSourceWeb = SourceWeb <$> (tag "SOURCEWEB" >> restOfTag)

data NewVariable = NewVariable { varName :: String
                               , startingValue :: Formula }
                 deriving (Eq, Show)

parseDefine :: PParser GlobalTag
parseDefine = do
  varName <- tag "DEFINE" *> parseString
  startingValue <- char '|' *> parseFormula
  return . Define $ NewVariable { .. }

data AbilityNature = Normal | Automatic | Virtual deriving (Eq, Show)

data Ability = Ability { abilityCategory :: String
                       , abilityNature :: AbilityNature
                       , abilityName :: String
                       , abilityRestrictions :: [Restriction] } deriving (Eq, Show)

-- ABILITY:x|y|z
--   x is ability category
--   y is ability nature
--   z is ability name or key
parseAbility :: PParser GlobalTag
parseAbility = do
  _ <- tag "ABILITY"
  abilityCategory <- parseWordandSpace
  abilityNature <- char '|' *> parseAbilityNature
  abilityName <- char '|' *> parseString
  abilityRestrictions <- option [] parseAdditionalRestrictions
  return $ AbilityTag Ability { .. } where
    parseWordandSpace = many1 $ satisfy $ inClass "-A-Za-z "
    parseAbilityNature = (labeled "NORMAL" >> return Normal)
                     <|> (labeled "AUTOMATIC" >> return Automatic)
                     <|> (labeled "VIRTUAL" >> return Virtual)

-- AUTO:LANG|x|x...
--   x is language, language type, ALL, LIST, CLEAR.
data AutoLanguage = Language String
                  | LanguageType String
                  | AllLanguages
                  | ListLanguages
                  | ClearLanguages
                  | Invert AutoLanguage
                    deriving (Show, Eq)

parseAutoLanguage :: PParser GlobalTag
parseAutoLanguage = labeled "AUTO:LANG|" >> (AutoLanguageTag <$> parseLanguages) where
  parseLanguages = LanguageType <$> (labeled "TYPE=" *> parseString)
               <|> (labeled "ALL" >> return AllLanguages)
               <|> (labeled "%LIST" >> return ListLanguages)
               <|> (labeled "CLEAR" >> return ClearLanguages)
               <|> Invert <$> (char '!' >> parseLanguages)
               <|> Language <$> parseString

-- not fully implemented
data ChooseLanguage = ChoiceLanguage String
                    | ChoiceLanguageType String
                      deriving (Show, Eq)

parseChooseLanguage :: PParser GlobalTag
parseChooseLanguage = do
  _ <- labeled "CHOOSE:LANG|"
  languages <- parseChoice `sepBy` char ','
  return $ ChooseLanguageTag languages where
    parseChoice = ChoiceLanguageType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceLanguage <$> parseString

-- not fully implemented
data ChooseSkill = ChoiceSkill String
                 | ChoiceSkillType String
                   deriving (Show, Eq)

parseChooseSkill :: PParser GlobalTag
parseChooseSkill = do
  _ <- labeled "CHOOSE:SKILL|"
  skills <- parseChoice `sepBy` char ','
  return $ ChooseSkillTag skills where
    parseChoice = ChoiceSkillType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceSkill <$> parseString

parseClassSkill :: PParser GlobalTag
parseClassSkill = do
  _ <- tag "CSKILL"
  cskills <- parseString `sepBy` char '|'
  return $ ClassSkill cskills

-- TODO: catchall

parseGlobalTags :: PParser GlobalTag
parseGlobalTags = parseKeyStat
              <|> parseUseUntrained
              <|> parseSortKey
              <|> parseSourcePage
              <|> parseSourceWeb
              <|> parseDefine
              <|> parseSelect
              <|> parseProductIdentity
              <|> parseOutputName
              <|> parseAbility
              <|> parseAutoLanguage
              <|> parseClassSkill
              <|> parseChooseLanguage
              <|> parseChooseSkill
