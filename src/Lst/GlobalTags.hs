{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.GlobalTags (GlobalTag, parseGlobal) where

import qualified Data.Map as M

import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (sepBy, many1, notFollowedBy, option)
import Text.Parsec.Prim (try, many)
import Control.Monad.State (get, put)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import JEPFormula
import Common

data GlobalTag = KeyStat String
               | UseUntrained Bool
               | SortKey String
               | SourcePage String
               | SourceWeb String
               | SourceLong String
               | SourceShort String
               | ProductIdentity Bool
               | DescriptionEmphasized Bool
               | OutputName String
               | AbilityTag Ability
               | VisionTag Vision
               | Select Formula
               | SpecialAbilityTag [String]
               | UnarmedDamage [String]
               | VirtualFeatTag VFeat
               | Define NewVariable
               | SpellResistance Formula
               | DamageReduction DamageReductionType
               | AddFeatTag AddFeat
               | AutoEquipTag [String]
               | AutoFeatTag AutoFeat
               | AutoLanguageTag AutoLanguage
               | AutoWeaponProfTag [AutoWeaponProf]
               | ClassSkill [ClassSkillType]
               | SpellTag Spell
               | ChooseLanguageTag [ChooseLanguage]
               | ChooseNumberTag Choices
               | Damage [Roll]
               | SecondaryDamage [Roll]
               | CriticalRange Int
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
parseSourcePage = tag "SOURCEPAGE" >> restOfTag

parseSourceWeb :: PParser String
parseSourceWeb = tag "SOURCEWEB" >> restOfTag

parseSourceLong :: PParser String
parseSourceLong = tag "SOURCELONG" >> restOfTag

parseSourceShort :: PParser String
parseSourceShort = tag "SOURCESHORT" >> restOfTag

parseProductIdentity :: PParser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseDescEmphasized :: PParser Bool
parseDescEmphasized = tag "DESCISPI" *> yesOrNo

parseOutputName :: PParser String
parseOutputName = tag "OUTPUTNAME" >> restOfTag

parseSelect :: PParser Formula
parseSelect = tag "SELECT" >> parseFormula

parseDamage :: PParser [Roll]
parseDamage = tag "DAMAGE" *> parseRolls

parseAltDamage :: PParser [Roll]
parseAltDamage = tag "ALTDAMAGE" *> parseRolls

parseSR :: PParser Formula
parseSR = tag "SR" *> parseFormula

data Vision = Vision { visionTypes :: [String]
                     , visionRestrictions :: [RestrictionTag] }
              deriving (Eq, Show)

parseVision :: PParser Vision
parseVision = do
  _ <- tag "VISION"
  visionTypes <- parseVisionString `sepBy` char '|'
  visionRestrictions <- option [] parseAdditionalRestrictions
  return Vision { .. } where
    parseVisionString = try (disallowed *> parseString)
    disallowed = notFollowedBy (string "PRE")

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
                       , abilityRestrictions :: [RestrictionTag] }
               deriving (Eq, Show)

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
    parseAbilityNature = (Normal <$ labeled "NORMAL")
                     <|> (Automatic <$ labeled "AUTOMATIC")
                     <|> (Virtual <$ labeled "VIRTUAL")
    parseAbilityString = try (char '|' *> disallowed *> parseString)
    -- TODO: ugly hack
    disallowed = notFollowedBy (string "PRE") *> notFollowedBy (string "!PRE")

-- ADD:FEAT|x|y,y...
data FeatType = AllFeats
              | FeatType String
              | FeatName String
                deriving (Show, Eq)

data AddFeat = AddFeat { featChoices :: Int
                       , featTypes :: [FeatType] }
             deriving (Show, Eq)

parseAddFeat :: PParser AddFeat
parseAddFeat = do
  _ <- labeled "ADD:FEAT|"
  featChoices <- option 1 (textToInt <$> manyNumbers)
  featTypes <- parseFeatTypes `sepBy` char ','
  return AddFeat { .. } where
    parseFeatTypes = (AllFeats <$ labeled "ALL")
                 <|> (labeled "TYPE=" >> FeatType <$> parseStringNoCommas)
                 <|> (FeatName <$> parseStringNoCommas)

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
               <|> (AllLanguages <$ labeled "ALL")
               <|> (ListLanguages <$ labeled "%LIST")
               <|> (ClearLanguages <$ labeled "CLEAR")
               <|> Invert <$> (char '!' >> parseLanguages)
               <|> Language <$> parseString

-- AUTO:EQUIP|x|x...
--   x is equipment name
parseAutoEquip :: PParser [String]
parseAutoEquip = do
  _ <- labeled "AUTO:EQUIP"
  many1 parseEquipmentText where
    parseEquipmentText = try (char '|' *> notFollowedBy (string "PRE") *> parseString)

-- AUTO:FEAT|x|x...
--   x is feat name
data AutoFeat = AutoFeat { featNames :: [String]
                         , featRestrictions :: [RestrictionTag] }
                deriving (Eq, Show)

parseAutoFeat :: PParser AutoFeat
parseAutoFeat = do
  _ <- labeled "AUTO:FEAT|"
  featNames <- parseString `sepBy` char '|'
  featRestrictions <- option [] parseAdditionalRestrictions
  return AutoFeat { .. }

-- AUTO:WEAPONPROF|x|x...
--   x is weapon name, type or deity's favored weapon
data AutoWeaponProf = WeaponName String
                    | WeaponType String
                    | WeaponOfDeity
                      deriving (Show, Eq)

parseAutoWeaponProf :: PParser [AutoWeaponProf]
parseAutoWeaponProf = do
  _ <- labeled "AUTO:WEAPONPROF|"
  parseAutoWeaponProfType `sepBy` char '|' where
    parseAutoWeaponProfType = (WeaponOfDeity <$ labeled "DEITYWEAPONS")
                      <|> (labeled "TYPE=" >> WeaponType <$> parseString)
                      <|> (labeled "TYPE." >> WeaponType <$> parseString)
                      <|> (WeaponName <$> parseString)

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
data Choices = Choices { choiceNumber :: Int
                       , choices :: [String]
                       , choiceType :: Maybe String }
                   deriving (Show, Eq)

parseChooseNumChoices :: PParser Choices
parseChooseNumChoices = do
  _ <- labeled "CHOOSE:NUMCHOICES="
  choiceNumber <- textToInt <$> manyNumbers
  choices <- many1 $ try (char '|' *> parseChoiceString)
  choiceType <- tryOption (labeled "|TYPE=" *> parseString)
  return Choices { .. } where
    parseChoiceString = disallowed *> parseString
    disallowed = notFollowedBy (string "TYPE")

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

data ClassSkillType = CSkillName String
                    | CSkillType String
                    | CSkillAll
                    | CSkillList
                      deriving (Show, Eq)

parseClassSkill :: PParser [ClassSkillType]
parseClassSkill = do
  _ <- tag "CSKILL"
  parseCSkill `sepBy` char '|' where
    parseCSkill = (CSkillAll <$ labeled "ALL")
              <|> (CSkillList <$ labeled "LIST")
              <|> (labeled "TYPE=" >> CSkillType <$> parseString)
              <|> CSkillName <$> parseString

-- DR:x/y
--   x is formula
--   y is damage type that bypasses this reduction
data DamageReductionType = DamageReductionType { damageReduction :: Int
                                               , damageReductionText :: String }
                          deriving (Show, Eq)

parseDR :: PParser DamageReductionType
parseDR = do
  _ <- tag "DR"
  -- can't parse as a formula; will eat up the '/'.
  damageReduction <- textToInt <$> manyNumbers
  _ <- char '/'
  damageReductionText <- parseString
  return DamageReductionType { .. }

parseSpecialAbilityName :: PParser [String]
parseSpecialAbilityName = do
  _ <- tag "SAB"
  parseStringNoCommasBrackets `sepBy` char '|'

-- SPELLS:s|u|v|w|x,y|x,y|z|z
--   s is name of spellbook (or .CLEARALL, not implemented)
--   u is times
--   v is time unit
--   w is caster level
--   x is spell name
--   y is formula
--   z is restrictions
data Spell = Spell { spellBook :: String
                   , spellTimes :: Formula
                   , spellTimeUnit :: String
                   , spellCasterLevel :: Formula
                   , spellNames :: [(String, Maybe Formula)]
                   , spellRestrictions :: [RestrictionTag] }
           deriving (Eq, Show)

parseSpells :: PParser Spell
parseSpells = do
  _ <- tag "SPELLS"
  spellBook <- parseString
  spellTimes <- option (Number 1) parseTimes
  spellTimeUnit <- option "Day" parseTimeUnit
  spellCasterLevel <- option (Number 1) parseCasterLevel
  spellNames <- many1 $ try (char '|' *> parseNames)
  spellRestrictions <- option [] parseAdditionalRestrictions
  return Spell { .. } where
    parseTimes = labeled "|TIMES=" >> parseFormula
    parseTimeUnit = labeled "|TIMEUNIT=" >> parseString
    parseCasterLevel = labeled "|CASTERLEVEL=" >> parseFormula
    -- prevent accidentally swallowing up restrictions.
    disallowed = notFollowedBy (string "PRE")
    parseNames = try parseNameAndDC <|> try parseNameOnly
    parseNameAndDC = do
      name <- disallowed *> parseStringNoCommas <* char ','
      spellDC <- parseFormula
      return (name, Just spellDC)
    parseNameOnly = do
      name <- disallowed *> parseStringNoCommas
      return (name, Nothing)

-- UDAM:x,x,x...
--   x is text (either 1 or 9)
parseUnarmedDamage :: PParser [String]
parseUnarmedDamage = tag "UDAM" *> parseStringNoCommas `sepBy` char ','

data VFeat = VFeat { vfeats :: [String]
                   , vfeatRestrictions :: [RestrictionTag] }
           deriving (Eq, Show)

parseVirtualFeat :: PParser VFeat
parseVirtualFeat = do
  _ <- tag "VFEAT"
  vfeat <- parseString
  vfeatRest <- try (many parseVFeat)
  vfeatRestrictions <- option [] parseAdditionalRestrictions
  let vfeats = vfeat : vfeatRest
  return VFeat { .. } where
    disallowed = notFollowedBy (string "|PRE")
    parseVFeat = disallowed *> char '|' *> parseString

parseUnknownTag :: PParser (String, String)
parseUnknownTag = do
  tagName <- allCaps <* char ':'
  rest <- restOfTag
  _ <- warning ("unknown tag: " ++ tagName ++ " with contents: " ++ rest) $ return ()
  return (tagName, rest)

parseGlobal :: PParser GlobalTag
parseGlobal = KeyStat <$> parseKeyStat
          <|> SortKey <$> parseSortKey
          <|> UseUntrained <$> parseUseUntrained
          <|> SourcePage <$> parseSourcePage
          <|> SourceWeb <$> parseSourceWeb
          <|> SourceShort <$> parseSourceShort
          <|> SourceLong <$> parseSourceLong
          <|> ProductIdentity <$> parseProductIdentity
          <|> DescriptionEmphasized <$> parseDescEmphasized
          <|> OutputName <$> parseOutputName
          <|> Select <$> parseSelect
          <|> Define <$> parseDefine
          <|> AbilityTag <$> parseAbility
          <|> AddFeatTag <$> parseAddFeat
          <|> AutoEquipTag <$> parseAutoEquip
          <|> AutoFeatTag <$> parseAutoFeat
          <|> AutoLanguageTag <$> parseAutoLanguage
          <|> AutoWeaponProfTag <$> parseAutoWeaponProf
          <|> Damage <$> parseDamage
          <|> SecondaryDamage <$> parseAltDamage
          <|> ChooseLanguageTag <$> parseChooseLanguage
          <|> ChooseNumberTag <$> parseChooseNumChoices
          <|> ChooseSkillTag <$> parseChooseSkill
          <|> ClassSkill <$> parseClassSkill
          <|> DamageReduction <$> parseDR
          <|> SpellResistance <$> parseSR
          <|> SpellTag <$> parseSpells
          <|> SpecialAbilityTag <$> parseSpecialAbilityName
          <|> UnarmedDamage <$> parseUnarmedDamage
          <|> VirtualFeatTag <$> parseVirtualFeat
          <|> VisionTag <$> parseVision
          <|> Unknown <$> parseUnknownTag -- must be the very LAST thing tried
