{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.Global (GlobalTag, parseGlobalTags) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Restrictions(Restriction, parseAdditionalRestrictions)
import Common

data GlobalTag = KeyStat T.Text
               | UseUntrained Bool
               | SourcePage T.Text
               | ProductIdentity Bool
               | OutputName T.Text
               | AbilityTag Ability
               | AutoLanguageTag AutoLanguage
               | ChooseLanguageTag [ChooseLanguage]
                 deriving (Eq, Show)

parseKeyStat :: Parser GlobalTag
parseKeyStat  = KeyStat <$> (tag "KEYSTAT" >> parseString)

parseUseUntrained :: Parser GlobalTag
parseUseUntrained = UseUntrained <$> (tag "USEUNTRAINED" >> yesOrNo)

parseSourcePage :: Parser GlobalTag
parseSourcePage  = SourcePage <$> (tag "SOURCEPAGE" >> parseString)

parseProductIdentity :: Parser GlobalTag
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

parseOutputName :: Parser GlobalTag
parseOutputName = OutputName <$> (tag "OUTPUTNAME" >> parseString)

data AbilityNature = Normal | Automatic | Virtual deriving (Eq, Show)

data Ability = Ability { abilityCategory :: T.Text
                       , abilityNature :: AbilityNature
                       , abilityName :: T.Text
                       , abilityRestrictions :: [Restriction] } deriving (Eq, Show)

-- ABILITY:x|y|z
--   x is ability category
--   y is ability nature
--   z is ability name or key
parseAbility :: Parser GlobalTag
parseAbility = do
  _ <- tag "ABILITY"
  abilityCategory <- parseWordandSpace
  abilityNature <- char '|' *> parseAbilityNature
  abilityName <- char '|' *> parseString
  abilityRestrictions <- option [] parseAdditionalRestrictions
  return $ AbilityTag Ability { .. } where
    parseWordandSpace = takeWhile1 $ inClass "-A-Za-z "
    parseAbilityNature = (string "NORMAL" >> return Normal)
                     <|> (string "AUTOMATIC" >> return Automatic)
                     <|> (string "VIRTUAL" >> return Virtual)

-- AUTO:LANG|x|x...
--   x is language, language type, ALL, LIST, CLEAR.
data AutoLanguage = Language T.Text
                  | LanguageType T.Text
                  | AllLanguages
                  | ListLanguages
                  | ClearLanguages
                  | Invert AutoLanguage
                    deriving (Show, Eq)

parseAutoLanguage :: Parser GlobalTag
parseAutoLanguage = string "AUTO:LANG|" >> (AutoLanguageTag <$> parseLanguages) where
  parseLanguages = LanguageType <$> (string "TYPE=" *> parseString)
               <|> (string "ALL" >> return AllLanguages)
               <|> (string "%LIST" >> return ListLanguages)
               <|> (string "CLEAR" >> return ClearLanguages)
               <|> Invert <$> (char '!' >> parseLanguages)
               <|> Language <$> parseString

-- not fully implemented
data ChooseLanguage = ChoiceLanguage T.Text
                    | ChoiceLanguageType T.Text
                      deriving (Show, Eq)

parseChooseLanguage :: Parser GlobalTag
parseChooseLanguage = do
  _ <- string "CHOOSE:LANG|"
  languages <- parseChoice `sepBy` char ','
  return $ ChooseLanguageTag languages where
    parseChoice = ChoiceLanguageType <$> (string "TYPE=" *> parseString)
              <|> ChoiceLanguage <$> parseString

-- TODO: catchall

parseGlobalTags :: Parser GlobalTag
parseGlobalTags = parseKeyStat
              <|> parseUseUntrained
              <|> parseSourcePage
              <|> parseProductIdentity
              <|> parseOutputName
              <|> parseAbility
              <|> parseAutoLanguage
              <|> parseChooseLanguage
