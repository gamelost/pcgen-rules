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
               | SpecialAbilityTag SpecialAbility
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
               | AutoShieldProfTag [AutoShieldProf]
               | AutoWeaponProfTag [AutoWeaponProf]
               | ClassSkill [ClassSkillType]
               | CrossClassSkill [CrossClassSkillType]
               | ChangeProficiency [ChangeProf]
               | CompanionListTag CompanionList
               | ServesAsTag ServesAs
               | SpellTag Spell
               | SpellsKnown [SpellKnown]
               | SpellLevelDomainTag SpellLevelDomain
               | SpellLevelClassTag SpellLevelClass
               | Damage [Roll]
               | SecondaryDamage [Roll]
               | FollowersTag Followers
               | CriticalRange Int
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

-- AUTO:SHIELDPROF|x|x...
--   x is weapon name, type or deity's favored weapon
data AutoShieldProf = ShieldName String
                    | ShieldType String
                      deriving (Show, Eq)

parseAutoShieldProf :: PParser [AutoShieldProf]
parseAutoShieldProf = do
  _ <- labeled "AUTO:SHIELDPROF|"
  parseAutoShieldProfType `sepBy` char '|' where
    parseAutoShieldProfType = (labeled "SHIELDTYPE=" >> ShieldType <$> parseString)
                          <|> (ShieldName <$> parseString)

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

-- CSKILL:x|x
--   x is skill name or type or ALL, LIST
data ClassSkillType = CSkillName String
                    | CSkillType String
                    | CSkillAll
                    | CSkillList
                      deriving (Show, Eq)

parseClassSkill :: PParser [ClassSkillType]
parseClassSkill = tag "CSKILL" *> parseCSkill `sepBy` char '|' where
    parseCSkill = (CSkillAll <$ labeled "ALL")
              <|> (CSkillList <$ labeled "LIST")
              <|> (labeled "TYPE=" >> CSkillType <$> parseString)
              <|> CSkillName <$> parseString

-- CCSKILL:x|x
--   x is skill name or type or LIST
data CrossClassSkillType = CCSkillName String
                         | CCSkillType String
                         | CCSkillList
                           deriving (Show, Eq)

parseCClassSkill :: PParser [CrossClassSkillType]
parseCClassSkill = tag "CCSKILL" *> parseCSkill `sepBy` char '|' where
    parseCSkill = (CCSkillList <$ labeled "LIST")
              <|> (labeled "TYPE=" >> CCSkillType <$> parseString)
              <|> CCSkillName <$> parseString

-- CHANGEPROF:x,x=y|x,x=y
--   x is weapon name or type
--   y is category of proficiency to change to
data ChangeProficiencyWeaponType = ProficiencyWeaponName String
                                 | ProficiencyWeaponType String
                                   deriving (Show, Eq)

data ChangeProf = ChangeProf { changeProficiencyWeaponType :: [ChangeProficiencyWeaponType]
                             , changeProficiencyWeaponTypeTo :: String }
                deriving (Show, Eq)

parseChangeProf :: PParser [ChangeProf]
parseChangeProf = tag "CHANGEPROF" *> parseChangeProfSeries `sepBy` char '|' where
  parseChangeProfSeries = do
    changeProficiencyWeaponType <- parseChangeType `sepBy` char ','
    _ <- char '='
    changeProficiencyWeaponTypeTo <- parseString
    return ChangeProf { .. }
  parseChangeType = (labeled "TYPE." *> (ProficiencyWeaponType <$> parseStringNoCommas))
                <|> (ProficiencyWeaponName <$> parseStringNoCommas)

-- COMPANIONLIST:x|y,y,...|z
--   x is companion type
--   y is companion race type
--   z is level adjustment (optional)
data CompanionRaceListType = CompanionRace String
                           | CompanionRaceType String
                           | CompanionRaceSubType String
                             deriving (Show, Eq)

data CompanionList = CompanionList { companionListType :: String
                                   , companionListRaceTypes :: [CompanionRaceListType]
                                   , followerAdjustment :: Maybe Int
                                   , companionRestrictions :: [RestrictionTag] }
                   deriving (Show, Eq)

parseCompanionList :: PParser CompanionList
parseCompanionList = do
  _ <- tag "COMPANIONLIST"
  companionListType <- parseString <* char '|'
  companionListRaceTypes <- parseRaceTypes `sepBy` char ','
  followerAdjustment <- tryOption (labeled "|FOLLOWERADJUSTMENT:" *> parseInteger)
  companionRestrictions <- option [] parseAdditionalRestrictions
  return CompanionList { .. } where
    parseRaceTypes = (labeled "RACETYPE=" *> (CompanionRaceType <$> parseStringNoCommas))
                 <|> (labeled "RACESUBTYPE=" *> (CompanionRaceSubType <$> parseStringNoCommas))
                 <|> (CompanionRace <$> parseStringNoCommas)

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

-- FOLLOWERS:x|y
--   x is text
--   y is formula
data Followers = Followers { followerType :: String
                           , followerFormula :: Formula }
               deriving (Show, Eq)

parseFollowers :: PParser Followers
parseFollowers = do
  _ <- tag "FOLLOWERS"
  followerType <- parseString <* char '|'
  followerFormula <- parseFormula
  return Followers { .. }

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

data SpecialAbilityType = SpecialAbilityDescription String
                        | SpecialAbilityFormula Formula
                        deriving (Show, Eq)

data SpecialAbility = SpecialAbility { specialAbilityType :: [SpecialAbilityType]
                                     , specialAbilityRestrictions :: [RestrictionTag] }
                    deriving (Show, Eq)

parseSpecialAbilityName :: PParser SpecialAbility
parseSpecialAbilityName = do
  _ <- tag "SAB"
  firstAbility <- SpecialAbilityDescription <$> untilEnd
  restAbilities <- many $ try (char '|' *> parseSpecialAbility)
  specialAbilityRestrictions <- option [] parseAdditionalRestrictions
  let specialAbilityType = firstAbility : restAbilities
  return SpecialAbility { .. } where
    parseSpecialAbility = try (notFollowedBy (string "PRE") *> parseSpecialAbilityType)
    parseSpecialAbilityType = try (SpecialAbilityFormula <$> parseFormula)
                          <|> try (SpecialAbilityDescription <$> untilEnd)
    untilEnd = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:;?!%#'()~+[]="

-- SERVESAS:x|y|y
--   x is ABILITY category, CLASS, RACE, or SKILL
--   y is object name
data ServesAsType = ServesAsAbility String
                  | ServesAsClass
                  | ServesAsRace
                  | ServesAsSkill
                    deriving (Show, Eq)

data ServesAs = ServesAs { servesAsType :: ServesAsType
                         , servesAsObjects :: [String] }
              deriving (Show, Eq)

parseServesAs :: PParser ServesAs
parseServesAs = do
  _ <- tag "SERVESAS"
  servesAsType <- parseServesAsType <* char '|'
  servesAsObjects <- parseString `sepBy` char '|'
  return ServesAs { .. } where
    parseServesAsType = (ServesAsClass <$ labeled "CLASS")
                    <|> (ServesAsSkill <$ labeled "SKILL")
                    <|> (ServesAsRace <$ labeled "RACE")
                    <|> (labeled "ABILITY=" *> (ServesAsAbility <$> parseString))

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

-- SPELLLEVEL:v|w=x|y|w=x|y|z|z
--   v is DOMAIN or CLASS
--   w is domain or class name or spell caster type
--   x is spell level number
--   y is spell list
--   z are restriction tags
data SpellLevelDomainList = SpellLevelDomainList { domainName :: String
                                                 , domainLevel :: Int
                                                 , domainSpell :: String }
                          deriving (Show, Eq)

data SpellLevelDomain = SpellLevelDomain { spellLevelDomainList :: [SpellLevelDomainList]
                                         , spellLevelDomainRestrictions :: [RestrictionTag] }
                      deriving (Show, Eq)

parseSpellLevelDomain :: PParser SpellLevelDomain
parseSpellLevelDomain = do
  _ <- labeled "SPELLLEVEL:DOMAIN|"
  spellLevelDomainList <- parseDomainList `sepBy` char '|'
  spellLevelDomainRestrictions <- option [] parseAdditionalRestrictions
  return SpellLevelDomain { .. } where
    parseDomainList = do
      domainName <- parseString <* char '='
      domainLevel <- parseInteger <* char '|'
      domainSpell <- parseString
      return SpellLevelDomainList { .. }

data SpellLevelClassType = SpellLevelClassName String
                         | SpellLevelSpellCaster String
                           deriving (Show, Eq)

data SpellLevelClassList = SpellLevelClassList { className :: SpellLevelClassType
                                               , classLevel :: Int
                                               , classSpell :: String }
                           deriving (Show, Eq)

data SpellLevelClass = SpellLevelClass { spellLevelClassList :: [SpellLevelClassList]
                                       , spellLevelClassRestrictions :: [RestrictionTag] }
                       deriving (Show, Eq)

parseSpellLevelClass :: PParser SpellLevelClass
parseSpellLevelClass = do
  _ <- labeled "SPELLLEVEL:CLASS|"
  spellLevelClassList <- parseClassList `sepBy` char '|'
  spellLevelClassRestrictions <- option [] parseAdditionalRestrictions
  return SpellLevelClass { .. } where
    parseClassList = do
      className <- parseClassType <* char '='
      classLevel <- parseInteger <* char '|'
      classSpell <- parseString
      return SpellLevelClassList { .. }
    parseClassType = (labeled "SPELLCASTER." *> (SpellLevelSpellCaster <$> parseString))
                 <|> (SpellLevelClassName <$> parseString)

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
          <|> AutoShieldProfTag <$> parseAutoShieldProf
          <|> AutoWeaponProfTag <$> parseAutoWeaponProf
          <|> Damage <$> parseDamage
          <|> SecondaryDamage <$> parseAltDamage
          <|> FollowersTag <$> parseFollowers
          <|> ClassSkill <$> parseClassSkill
          <|> CrossClassSkill <$> parseCClassSkill
          <|> ChangeProficiency <$> parseChangeProf
          <|> CompanionListTag <$> parseCompanionList
          <|> ServesAsTag <$> parseServesAs
          <|> DamageReduction <$> parseDR
          <|> SpellResistance <$> parseSR
          <|> SpellTag <$> parseSpells
          <|> SpellsKnown <$> parseSpellsKnownClass
          <|> SpellLevelDomainTag <$> parseSpellLevelDomain
          <|> SpellLevelClassTag <$> parseSpellLevelClass
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
