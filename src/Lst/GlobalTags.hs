{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.GlobalTags (GlobalTag, parseGlobalTags) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (State, (<|>))
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
               | SpecialAbilityTag String
               | VirtualFeatTag [String]
               | Define NewVariable
               | AutoEquipTag [String]
               | AutoLanguageTag AutoLanguage
               | ClassSkill [String]
               | ChooseLanguageTag [ChooseLanguage]
               | ChooseSkillTag [ChooseSkill]
               | Unknown (String, String)
                 deriving (Eq, Show)

parseSortKey :: PParser String
parseSortKey = tag "SORTKEY" >> restOfTag

parseKeyStat :: PParser String
parseKeyStat = tag "KEYSTAT" >> restOfTag

parseUseUntrained :: PParser Bool
parseUseUntrained = tag "USEUNTRAINED" >> yesOrNo

parseSourcePage :: PParser String
parseSourcePage  = tag "SOURCEPAGE" >> restOfTag

parseProductIdentity :: PParser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseOutputName :: PParser String
parseOutputName = tag "OUTPUTNAME" >> restOfTag

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
  varName <- tag "DEFINE" *> parseTill '|'
  varFormula <- parseFormula
  vars <- get
  let varValue = evalJEPFormula vars varFormula
  put $ M.insert varName varValue vars
  return NewVariable { .. }

data AbilityNature = Normal | Automatic | Virtual deriving (Eq, Show)

data Ability = Ability { abilityCategory :: String
                       , abilityNature :: AbilityNature
                       , abilityNames :: [String]
                       , abilityRestrictions :: [Restriction] } deriving (Eq, Show)

-- ABILITY:x|y|z|z|...
--   x is ability category
--   y is ability nature
--   z is ability name or key
parseAbility :: PParser Ability
parseAbility = do
  _ <- tag "ABILITY"
  abilityCategory <- parseTill '|'
  abilityNature <- parseAbilityNature
  abilityNames <- many1 parseAbilityString
  abilityRestrictions <- option [] parseAdditionalRestrictions
  return Ability { .. } where
    parseAbilityNature = (labeled "NORMAL" >> return Normal)
                     <|> (labeled "AUTOMATIC" >> return Automatic)
                     <|> (labeled "VIRTUAL" >> return Virtual)
    parseAbilityString = try (char '|' *> notFollowedBy (string "PRE") *> parseString)

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

-- AUTO:EQUIP|x|x...
--   x is equipment name
parseAutoEquip :: PParser [String]
parseAutoEquip = do
  _ <- labeled "AUTO:EQUIP"
  many1 parseEquipmentText where
    parseEquipmentText = try (char '|' *> notFollowedBy (string "PRE") *> parseString)

-- not fully implemented
data ChooseLanguage = ChoiceLanguage String
                    | ChoiceLanguageType String
                      deriving (Show, Eq)

parseChooseLanguage :: PParser [ChooseLanguage]
parseChooseLanguage = do
  _ <- labeled "CHOOSE:LANG|"
  parseChoice `sepBy` char ',' where
    parseChoice = ChoiceLanguageType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceLanguage <$> parseString

-- not fully implemented
data ChooseSkill = ChoiceSkill String
                 | ChoiceSkillType String
                   deriving (Show, Eq)

parseChooseSkill :: PParser [ChooseSkill]
parseChooseSkill = do
  _ <- labeled "CHOOSE:SKILL|"
  parseChoice `sepBy` char ',' where
    parseChoice = ChoiceSkillType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceSkill <$> parseString

parseClassSkill :: PParser [String]
parseClassSkill = do
  _ <- tag "CSKILL"
  parseString `sepBy` char '|'

parseSpecialAbilityName :: PParser String
parseSpecialAbilityName = do
  _ <- tag "SAB"
  parseStringNoCommasBrackets

parseVirtualFeat :: PParser [String]
parseVirtualFeat = tag "VFEAT" *> parseString `sepBy` char '|'

parseUnknownTag :: PParser (String, String)
parseUnknownTag = do
  tagName <- allCaps <* char ':'
  rest <- restOfTag
  _ <- warning ("unknown tag: " ++ tagName ++ " with contents: " ++ rest) $ return ()
  return (tagName, rest)

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
              <|> AutoEquipTag <$> parseAutoEquip
              <|> AutoLanguageTag <$> parseAutoLanguage
              <|> ChooseLanguageTag <$> parseChooseLanguage
              <|> ChooseSkillTag <$> parseChooseSkill
              <|> ClassSkill <$> parseClassSkill
              <|> SpecialAbilityTag <$> parseSpecialAbilityName
              <|> VirtualFeatTag <$> parseVirtualFeat
              <|> Unknown <$> parseUnknownTag -- must be the very LAST thing tried
