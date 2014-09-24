{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.GlobalTags (GlobalTag, parseGlobalTags) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import Restrictions(Restriction, parseAdditionalRestrictions)
import Control.Monad.State
import qualified Data.Map as M
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
               | SpecialAbilityTag SpecialAbility
               | VirtualFeatTag [String]
               | Define NewVariable
               | AutoLanguageTag AutoLanguage
               | ClassSkill [String]
               | ChooseLanguageTag [ChooseLanguage]
               | ChooseSkillTag [ChooseSkill]
                 deriving (Eq, Show)

parseSortKey :: PParser String
parseSortKey = tag "SORTKEY" >> parseString

parseKeyStat :: PParser String
parseKeyStat = tag "KEYSTAT" >> parseString

parseUseUntrained :: PParser Bool
parseUseUntrained = tag "USEUNTRAINED" >> yesOrNo

parseSourcePage :: PParser String
parseSourcePage  = tag "SOURCEPAGE" >> parseString

parseProductIdentity :: PParser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseOutputName :: PParser String
parseOutputName = tag "OUTPUTNAME" >> parseString

parseSelect :: PParser Formula
parseSelect = tag "SELECT" >> parseFormula

parseSourceWeb :: PParser String
parseSourceWeb = tag "SOURCEWEB" >> restOfTag

data NewVariable = NewVariable { varName :: String
                               , varFormula :: Formula
                               , varValue :: Int}
                 deriving (Eq, Show)

parseDefine :: PParser NewVariable
parseDefine = do
  varName <- tag "DEFINE" *> parseString
  varFormula <- char '|' *> parseFormula
  vars <- get
  let varValue = evalJEPFormula vars varFormula
  put $ M.insert varName varValue vars
  return $ NewVariable { .. }

data AbilityNature = Normal | Automatic | Virtual deriving (Eq, Show)

data Ability = Ability { abilityCategory :: String
                       , abilityNature :: AbilityNature
                       , abilityName :: String
                       , abilityRestrictions :: [Restriction] } deriving (Eq, Show)

-- ABILITY:x|y|z
--   x is ability category
--   y is ability nature
--   z is ability name or key
parseAbility :: PParser Ability
parseAbility = do
  _ <- tag "ABILITY"
  abilityCategory <- parseWordandSpace
  abilityNature <- char '|' *> parseAbilityNature
  abilityName <- char '|' *> parseString
  abilityRestrictions <- option [] parseAdditionalRestrictions
  return $ Ability { .. } where
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

parseAutoLanguage :: PParser AutoLanguage
parseAutoLanguage = labeled "AUTO:LANG|" >> parseLanguages where
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

parseChooseLanguage :: PParser [ChooseLanguage]
parseChooseLanguage = do
  _ <- labeled "CHOOSE:LANG|"
  languages <- parseChoice `sepBy` char ','
  return languages where
    parseChoice = ChoiceLanguageType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceLanguage <$> parseString

-- not fully implemented
data ChooseSkill = ChoiceSkill String
                 | ChoiceSkillType String
                   deriving (Show, Eq)

parseChooseSkill :: PParser [ChooseSkill]
parseChooseSkill = do
  _ <- labeled "CHOOSE:SKILL|"
  skills <- parseChoice `sepBy` char ','
  return skills where
    parseChoice = ChoiceSkillType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceSkill <$> parseString

parseClassSkill :: PParser [String]
parseClassSkill = do
  _ <- tag "CSKILL"
  cskills <- parseString `sepBy` char '|'
  return cskills

data SpecialAbility = SpecialAbilityName String
                    | ClearAbilityName String
                    | ClearAbility
                    deriving (Show, Eq)

parseSpecialAbilityName :: PParser SpecialAbility
parseSpecialAbilityName = do
  _ <- tag "SAB"
  ability <- parseSpecialAbility
  return ability where
    parseSpecialAbility = ClearAbilityName <$> (labeled ".CLEAR." >> parseStringNoCommas)
                      <|> (labeled ".CLEAR" >> return ClearAbility)
                      <|> SpecialAbilityName <$> parseStringNoCommas
    parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()~"

parseVirtualFeat :: PParser [String]
parseVirtualFeat = tag "VFEAT" *> parseString `sepBy` char '|'

-- TODO: catchall

parseGlobalTags :: PParser GlobalTag
parseGlobalTags = KeyStat <$> parseKeyStat
              <|> SortKey <$> parseSortKey
              <|> UseUntrained <$> parseUseUntrained
              <|> SourcePage <$> parseSourcePage
              <|> ProductIdentity <$> parseProductIdentity
              <|> OutputName <$> parseOutputName
              <|> Select <$> parseSelect
              <|> SourceWeb <$> parseSourceWeb
              <|> Define <$> parseDefine
              <|> AbilityTag <$> parseAbility
              <|> AutoLanguageTag <$> parseAutoLanguage
              <|> ChooseLanguageTag <$> parseChooseLanguage
              <|> ChooseSkillTag <$> parseChooseSkill
              <|> ClassSkill <$> parseClassSkill
              <|> SpecialAbilityTag <$> parseSpecialAbilityName
              <|> VirtualFeatTag <$> parseVirtualFeat
