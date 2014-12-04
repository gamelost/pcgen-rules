{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.GlobalTags (GlobalTag, parseGlobal) where

import qualified Data.Map as M

import Text.Parsec.Char (char, string, satisfy)
import Text.Parsec.Combinator (sepBy, many1, notFollowedBy, option, optional)
import Text.Parsec.Prim (try, many)
import Control.Monad.State (get, put)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import JEPFormula
import Common

data GlobalTag = KeyStat String
               | Key String
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
               | MoveTag [Move]
               | VisionTag Vision
               | Select Formula
               | SpecialAbilityTag [String]
               | UnarmedDamage [String]
               | NaturalAttacksTag NaturalAttacks
               | DefineStat DefineStatScore
               | VirtualFeatTag VFeat
               | Define NewVariable
               | SpellResistance Formula
               | DamageReduction DamageReductionType
               | AddFeatTag AddFeat
               | AddLanguageTag AddLanguage
               | AddSpellCasterTag AddSpellCaster
               | AutoArmorProfTag [AutoArmorProf]
               | AutoEquipTag [String]
               | AutoFeatTag AutoFeat
               | AutoLanguageTag AutoLanguage
               | AutoWeaponProfTag [AutoWeaponProf]
               | ClassSkill [ClassSkillType]
               | SpellTag Spell
               | SpellsKnown [SpellKnown]
               | ChooseLanguageTag [ChooseLanguage]
               | ChooseNumberChoicesTag Choices
               | ChooseNumberTag ChooseNumber
               | ChooseManyNumbersTag ChooseManyNumbers
               | ChooseNoChoice ()
               | Damage [Roll]
               | SecondaryDamage [Roll]
               | CriticalRange Int
               | ChooseSkillTag [ChooseSkill]
               | KitTag Kit
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

-- used in Spell lst types
parseSourceLink :: PParser String
parseSourceLink = tag "SOURCELINK" >> restOfTag

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

parseKey :: PParser String
parseKey = tag "KEY" *> restOfTag

data Kit = Kit { kitNumber :: Int
               , kitChoices :: [String] }
         deriving (Eq, Show)

parseKit :: PParser Kit
parseKit = do
  _ <- tag "KIT"
  kitNumber <- parseInteger <* char '|'
  kitChoices <- parseStringNoCommas `sepBy` char ','
  return Kit { .. }

data Move = Move { moveMode :: String
                 , moveRate :: Formula }
          deriving (Eq, Show)

parseMove :: PParser [Move]
parseMove = tag "MOVE" *> parseMoves `sepBy` char ',' where
  parseMoves = do
    moveMode <- parseStringNoCommas <* char ','
    moveRate <- parseFormula
    return Move { .. }

data Vision = Vision { visionTypes :: [String]
                     , visionRestrictions :: [RestrictionTag] }
              deriving (Eq, Show)

parseVision :: PParser Vision
parseVision = do
  _ <- tag "VISION"
  visionTypes <- many1 $ parseVisionString <* optional (char '|')
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

-- ADD:LANGUAGE|x|y,y...
--   x is optional formula
--   y is language name, type, or ALL
data AddLanguageType = LanguageName String
                     | LanguageType String
                     | AllLanguages
                       deriving (Show, Eq)

data AddLanguage = AddLanguage { languageNumChoices :: Formula
                               , languageTypes :: [AddLanguageType] }
                 deriving (Show, Eq)

parseAddLanguage :: PParser AddLanguage
parseAddLanguage = do
  _ <- labeled "ADD:LANGUAGE|"
  languageNumChoices <- option (Number 1) $ parseFormula <* char '|'
  languageTypes <- parseLanguageTypes `sepBy` char ','
  return AddLanguage { .. } where
    parseLanguageTypes = (labeled "TYPE=" >> LanguageType <$> parseStringNoCommas)
                     <|> (AllLanguages <$ labeled "ALL")
                     <|> (LanguageName <$> parseStringNoCommas)

-- ADD:SPELLCASTER|x|y,y
--   x is optional formula
--   y is class name, spellcasting class type, or ANY
data SpellCasterType = AnySpellClass
                     | SpellType String
                       deriving (Show, Eq)

data AddSpellCaster = AddSpellCaster { spellCasterBonus :: Formula
                                     , spellCasterTypes :: [SpellCasterType] }
                    deriving (Show, Eq)

parseAddSpellCaster :: PParser AddSpellCaster
parseAddSpellCaster = do
  _ <- labeled "ADD:SPELLCASTER|"
  spellCasterBonus <- option (Number 1) $ parseFormula <* char '|'
  spellCasterTypes <- parseSpellCasterType `sepBy` char ','
  return AddSpellCaster { .. } where
    parseSpellCasterType = (AnySpellClass <$ labeled "ANY")
                       <|> (SpellType <$> parseString)

-- AUTO:ARMORPROF|x|x...
--   x is armor name or armor type
data AutoArmorProf = ArmorName String
                   | ArmorType String
                     deriving (Show, Eq)

parseAutoArmorProf :: PParser [AutoArmorProf]
parseAutoArmorProf = labeled "AUTO:ARMORPROF|" >> parseAutoArmor `sepBy` char '|' where
  parseAutoArmor = (labeled "ARMORTYPE=" >> ArmorType <$> parseString)
               <|> (ArmorName <$> parseString)

-- AUTO:LANG|x|x...
--   x is language, language type, ALL, LIST, CLEAR.
data AutoLanguage = Language String
                  | AutoLanguageType String
                  | AutoAllLanguages
                  | ListLanguages
                  | ClearLanguages
                  | Invert AutoLanguage
                    deriving (Show, Eq)

parseAutoLanguage :: PParser AutoLanguage
parseAutoLanguage = labeled "AUTO:LANG|" >> parseLanguages where
  parseLanguages = AutoLanguageType <$> (labeled "TYPE=" *> parseString)
               <|> (AutoAllLanguages <$ labeled "ALL")
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

parseChooseNoChoice :: PParser ()
parseChooseNoChoice = () <$ labeled "CHOOSE:NOCHOICE"

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
data ChooseNumber = ChooseNumber { chooseMin :: Int
                                 , chooseMax :: Int
                                 , chooseTitle :: String }
                    deriving (Show, Eq)

parseChooseNumber :: PParser ChooseNumber
parseChooseNumber = do
  _ <- labeled "CHOOSE:NUMBER"
  chooseMin <- labeled "|MIN=" *> parseInteger
  chooseMax <- labeled "|MAX=" *> parseInteger
  chooseTitle <- labeled "|TITLE=" *> parseStringSemicolon
  return ChooseNumber { .. } where
   parseStringSemicolon = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~;"

data ChooseManyNumbers = ChooseManyNumbers { chooseManyNumbers :: [Int]
                                           , chooseManyMultiple :: Bool
                                           , chooseManyTitle :: String }
                       deriving (Show, Eq)

parseChooseManyNumbers :: PParser ChooseManyNumbers
parseChooseManyNumbers = do
  _ <- labeled "CHOOSE:NUMBER"
  chooseManyNumbers <- parseInteger `sepBy` char '|'
  chooseManyMultiple <- option False (True <$ labeled "MULTIPLE|")
  chooseManyTitle <- labeled "|TITLE=" *> parseString
  return ChooseManyNumbers { .. }

-- not fully implemented
data ChooseSkill = ChoiceSkill String
                 | ChoiceSkillType String
                 | ChoiceSkillTitle String
                   deriving (Show, Eq)

parseChooseSkill :: PParser [ChooseSkill]
parseChooseSkill = do
  _ <- labeled "CHOOSE:SKILL|"
  parseChoice `sepBy` char '|' where
    parseChoice = ChoiceSkillType <$> (labeled "TYPE=" *> parseString)
              <|> ChoiceSkillTitle <$> (labeled "TITLE=" *> parseString)
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

-- DEFINESTAT:x|y|z
--   x is LOCK, UNLOCK, NONSTAT, STAT, MINVALUE, MAXVALUE
--   y is stat name
--   z is formula
data DefineStatType = Lock
                    | Unlock
                    | NonStat
                    | Stat
                    | MinValue
                    | MaxValue
                      deriving (Show, Eq)

data DefineStatScore = DefineStatScore { defineStatType :: DefineStatType
                                       , defineStatName :: String
                                       , defineStatFormula :: Maybe Formula }
                     deriving (Show, Eq)

-- specially handle NONSTAT.
parseDefineNonStat :: PParser DefineStatScore
parseDefineNonStat = do
  _ <- labeled "DEFINESTAT:NONSTAT|"
  defineStatName <- parseString
  let defineStatType = NonStat
  let defineStatFormula = Nothing
  return DefineStatScore { .. }

parseDefineStat :: PParser DefineStatScore
parseDefineStat = do
  _ <- tag "DEFINESTAT"
  defineStatType <- parseDefineStatType
  defineStatName <- char '|' *> parseString
  defineStatFormula <- Just <$> (char '|' *> parseFormula)
  return DefineStatScore { .. } where
    parseDefineStatType = (Lock <$ labeled "LOCK")
                      <|> (Unlock <$ labeled "UNLOCK")
                      <|> (Stat <$ labeled "STAT")
                      <|> (MinValue <$ labeled "MINVALUE")
                      <|> (MaxValue <$ labeled "MAXVALUE")

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

-- NATURALATTACKS:v,w.w,x,y,z|v,w.w,x,y,z
--   v is natural weapon name
--   w is natural weapon type
--   x is number of attacks
--   y is natural weapon damage
--   z is optional special property description
data NaturalWeaponAttackNumber = DependsOnManyWeapons Int
                               | AttackProgression Int
                                 deriving (Show, Eq)

data NaturalAttacks = NaturalAttacks { naturalWeaponName :: String
                                     , naturalWeaponTypes :: [String]
                                     , naturalWeaponAttackNumber :: NaturalWeaponAttackNumber
                                     , naturalWeaponDamage :: [Roll]
                                     , naturalWeaponSpecialDescription :: Maybe String }
                      deriving (Show, Eq)

parseNaturalAttacks :: PParser NaturalAttacks
parseNaturalAttacks = do
  _ <- tag "NATURALATTACKS"
  naturalWeaponName <- parseStringNoCommas <* char ','
  naturalWeaponTypes <- parseStringNoPeriods `sepBy` char '.'
  _ <- char ','
  naturalWeaponAttackNumber <- parseAttackNumber <* char ','
  naturalWeaponDamage <- parseRolls
  naturalWeaponSpecialDescription <- tryOption $ char ',' *> restOfTag
  return NaturalAttacks { .. } where
    parseAttackNumber = char '*' *> (DependsOnManyWeapons <$> parseInt)
                    <|> AttackProgression <$> parseInt
    parseInt = textToInt <$> manyNumbers
    parseStringNoPeriods = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+/:?!%#'()[]~"

parseSpecialAbilityName :: PParser [String]
parseSpecialAbilityName = do
  _ <- tag "SAB"
  parseStringNoBrackets `sepBy` char '|' where
    -- allow commas.
    parseStringNoBrackets = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()~"

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

-- SPELLKNOWN:CLASS|w=x|y|w=x|y|z|z
--   w is class name or spellcaster type
--   x is spell number
--   y is spell list
--   z is PRExxx tag
data SpellKnownType = SpellCaster String
                    | ClassName String
                      deriving (Show, Eq)

data SpellKnown = SpellKnown { spellKnownType :: SpellKnownType
                             , spellKnownLevel :: Int
                             , spellKnowns :: [String] }
                 deriving (Show, Eq)

parseSpellsKnownClass :: PParser [SpellKnown]
parseSpellsKnownClass = do
  _ <- labeled "SPELLKNOWN:CLASS"
  many1 $ char '|' *> parseSpellKnown where
    parseSpellKnown = do
      spellKnownType <- parseSpellType <* char '='
      spellKnownLevel <- textToInt <$> manyNumbers
      _ <- char '|'
      spellKnowns <- parseStringNoCommas `sepBy` char ','
      return SpellKnown { .. }
    parseSpellType = labeled "SPELLCASTER." *> (SpellCaster <$> parseString)
                 <|> (ClassName <$> parseString)

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
          <|> Key <$> parseKey
          <|> SortKey <$> parseSortKey
          <|> UseUntrained <$> parseUseUntrained
          <|> SourcePage <$> parseSourcePage
          <|> SourceWeb <$> parseSourceWeb
          <|> SourceWeb <$> parseSourceLink
          <|> SourceShort <$> parseSourceShort
          <|> SourceLong <$> parseSourceLong
          <|> ProductIdentity <$> parseProductIdentity
          <|> DescriptionEmphasized <$> parseDescEmphasized
          <|> OutputName <$> parseOutputName
          <|> Select <$> parseSelect
          <|> Define <$> parseDefine
          <|> AbilityTag <$> parseAbility
          <|> AddFeatTag <$> parseAddFeat
          <|> AddLanguageTag <$> parseAddLanguage
          <|> AddSpellCasterTag <$> parseAddSpellCaster
          <|> AutoArmorProfTag <$> parseAutoArmorProf
          <|> AutoEquipTag <$> parseAutoEquip
          <|> AutoFeatTag <$> parseAutoFeat
          <|> AutoLanguageTag <$> parseAutoLanguage
          <|> AutoWeaponProfTag <$> parseAutoWeaponProf
          <|> Damage <$> parseDamage
          <|> SecondaryDamage <$> parseAltDamage
          <|> ChooseLanguageTag <$> parseChooseLanguage
          <|> ChooseNumberChoicesTag <$> parseChooseNumChoices
          -- if this CHOOSE:NUMBER fails, try the next one
          <|> try (ChooseNumberTag <$> parseChooseNumber)
          <|> try (ChooseManyNumbersTag <$> parseChooseManyNumbers)
          <|> ChooseSkillTag <$> parseChooseSkill
          <|> ChooseNoChoice <$> parseChooseNoChoice
          <|> ClassSkill <$> parseClassSkill
          <|> DamageReduction <$> parseDR
          <|> SpellResistance <$> parseSR
          <|> SpellTag <$> parseSpells
          <|> SpellsKnown <$> parseSpellsKnownClass
          <|> SpecialAbilityTag <$> parseSpecialAbilityName
          <|> UnarmedDamage <$> parseUnarmedDamage
          <|> NaturalAttacksTag <$> parseNaturalAttacks
          <|> DefineStat <$> parseDefineNonStat
          <|> DefineStat <$> parseDefineStat
          <|> VirtualFeatTag <$> parseVirtualFeat
          <|> MoveTag <$> parseMove
          <|> VisionTag <$> parseVision
          <|> KitTag <$> parseKit
          <|> Unknown <$> parseUnknownTag -- must be the very LAST thing tried
