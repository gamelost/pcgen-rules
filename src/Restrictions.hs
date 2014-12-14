{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Restrictions where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, option, many1, choice, notFollowedBy)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

import JEPFormula
import Common

data RestrictionTag = PreClassRestriction PreClass
                    | PreClassSkillRestriction PreClassSkill
                    | PreVarRestriction PreVar
                    | PreAlignRestriction PreAlign
                    | PreEquipRestriction PreEquip
                    | PreEquipBothRestriction PreEquip
                    | PreEquipTwoWeaponRestriction PreEquip
                    | PreEquipPrimaryRestriction PreEquip
                    | PreEquipSecondaryRestriction PreEquip
                    | PreAbilityRestriction PreAbility
                    | PreDeityRestriction PreDeity
                    | PreDomainRestriction PreDomain
                    | PreTemplateRestriction PreTemplate
                    | PreTotalAttackBonus Int
                    | PreTextRestriction String
                    | PreFeatRestriction PreFeat
                    | PreItemRestriction PreItem
                    | PreRaceRestriction PreRace
                    | PreMoveRestriction PreMove
                    | PreStatRestriction PreStat
                    | PreTypeRestriction PreType
                    | PreSizeRestriction PreSize
                    | PreLegsRestriction PreLegs
                    | PreLangRestriction PreLang
                    | PreSpecialAbilityRestriction PreSA
                    | PreSpellResistanceRestriction PreSR
                    | PreDamageResistanceRestriction PreDR
                    | PreHandsRestriction PreHands
                    | PreRegionRestriction String
                    | PreSubClassRestriction PreSubClass
                    | PreAttackRestriction PreAttack
                    | PreSkillRestriction PreSkill
                    | PreGenderRestriction PreGender
                    | PreLevelRestriction PreLevel
                    | PreLevelMaxRestriction Int
                    | PreSpellRestriction PreSpell
                    | PreSpellBookRestriction Bool
                    | PreSpellCastRestriction [PreSpellCast]
                    | PreSpellDescriptorRestriction PreSpellDescriptor
                    | PreSpellSchoolRestriction PreSpellSchool
                    | PreSpellSchoolSubRestriction PreSpellSchoolSub
                    | PreSpellTypeRestriction PreSpellType
                    | PreProficencyArmorRestriction PreProficiency
                    | PreProficencyShieldRestriction PreProficiency
                    | PrePCLevelRestriction PrePCLevel
                    | PreSkillTotalRestriction PreSkillTot
                    | PreVisionRestriction PreVision
                    | PreWeaponProfRestriction PreWeaponProf
                    | PreWieldRestriction PreWield
                    | PreRuleRestriction PreRule
                    | PreCheckBaseRestriction PreCheckBase
                    | PreMultipleRestriction PreMult
                    | Invert RestrictionTag
                      deriving (Show, Eq)

-- helpers
data Operator = OpEqual
              | OpGreaterThan
              | OpGreaterThanOrEqual
              | OpLesserThan
              | OpLesserThanOrEqual
              | OpNotEqual
                deriving (Show, Eq)

operatorPrefixes :: [PParser String]
operatorPrefixes = tryStrings ["EQ", "GTEQ", "GT", "LTEQ", "LT", "NEQ"]

convertOperator :: String -> Operator
convertOperator "EQ" = OpEqual
convertOperator "GTEQ" = OpGreaterThanOrEqual
convertOperator "GT" = OpGreaterThan
convertOperator "LTEQ" = OpLesserThanOrEqual
convertOperator "LT" = OpLesserThan
convertOperator "NEQ" = OpNotEqual
convertOperator _ = error "invalid operator"

-- parse s=n where s is a string, n an integer
parseCheckValues :: PParser (String, Int)
parseCheckValues = do
  check <- parseString <* char '='
  value <- parseInteger
  return (check, value)

-- PREABILITY:x,CATEGORY=y,z,z,z...
--   x is the number of abilities needed
--   y is category name or ALL
--   z is ability name, ability type (TYPE.z), or ALL
data PreAbilityType = AbilityName String
                    | AbilityType String
                    | AllAbilities
                      deriving (Show, Eq)

data PreAbility = PreAbility { abilityNumber :: Int
                             , categoryName :: String
                             , abilities :: [PreAbilityType] }
                  deriving (Show, Eq)

parsePreAbility :: PParser PreAbility
parsePreAbility = do
  abilityNumber <- tag "PREABILITY" *> parseInteger
  categoryName <- labeled ",CATEGORY=" >> parseWordWithSpaces
  abilities <- char ',' >> parseAbilities `sepBy` char ','
  return PreAbility { .. } where
    parseWordWithSpaces = many1 $ satisfy $ inClass "-A-Za-z "
    parseAbilities = (labeled "TYPE." *> (AbilityType <$> parseStringNoCommasBrackets))
                 <|> AllAbilities <$ labeled "ALL"
                 <|> AbilityName <$> parseStringNoCommasBrackets

-- PREALIGN:x,x...
--   x is alignment abbreviation or alignment array number
data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE | None | Deity
                 deriving (Show, Eq)

data PreAlign = PreAlign { alignments :: [Alignment] }
                deriving (Show, Eq)

parsePreAlign :: PParser PreAlign
parsePreAlign = do
  args <- tag "PREALIGN" >> parseWord `sepBy` char ','
  return PreAlign { alignments = map parseAlignment args } where
    parseAlignment :: String -> Alignment
    parseAlignment x | x == "LG", x == "0" = LG
    parseAlignment x | x == "LN", x == "1" = LN
    parseAlignment x | x == "LE", x == "2" = LE
    parseAlignment x | x == "NG", x == "3" = NG
    parseAlignment x | x == "TN", x == "4" = TN
    parseAlignment x | x == "NE", x == "5" = NE
    parseAlignment x | x == "CG", x == "6" = CG
    parseAlignment x | x == "CN", x == "7" = CN
    parseAlignment x | x == "CE", x == "8" = CE
    parseAlignment x | x == "Deity", x == "10" = Deity
    parseAlignment _ = None

-- PREATTACK:x
--   x is base attack bonus number
data PreAttack = PreAttack { preAttackNumber :: Int }
               deriving (Show, Eq)

parsePreAttack :: PParser PreAttack
parsePreAttack = do
  _ <- tag "PREATT"
  preAttackNumber <- parseInteger
  return PreAttack { .. }

-- PRECHECKBASE:x,y=z,y=z...
--   x is number of base checks to succeed
--   y is a check name (from statsandchecks.lst)
--   z is number to compare to
data PreCheckBase = PreCheckBase { preCheckBaseNumber :: Int
                                 , preCheckBaseChecks :: [(String, Int)] }
                  deriving (Show, Eq)

parsePreCheckBase :: PParser PreCheckBase
parsePreCheckBase = do
  _ <- tag "PRECHECKBASE"
  preCheckBaseNumber <- parseInteger <* char ','
  preCheckBaseChecks <- parseCheckValues `sepBy` char ','
  return PreCheckBase { .. }

-- PRECLASS:x,y=z,y=z,y=z...
--   x is number of classes to pass
--   y is class name or class type (TYPE.y) or SPELLCASTER. or SPELLCASTER.y
--   z is number, class level
data PreClass = PreClass { passNumber :: Int
                         , classRequisites :: [(String, Int)] }
                deriving (Show, Eq)

parsePreClass :: PParser PreClass
parsePreClass = do
  n <- tag "PRECLASS" >> manyNumbers
  classRequisites <- char ',' >> parseCheckValues `sepBy` char ','
  return PreClass { passNumber = textToInt n, .. }

-- PRECSKILL:x,y
--   x is number of class skills
--   y is skill name or skill type (TYPE=y)
data ClassSkill = ClassSkillName String
                | ClassSkillType String
                  deriving (Show, Eq)

data PreClassSkill = PreClassSkill { classSkillNumber :: Int
                                   , classSkill :: ClassSkill }
                     deriving (Show, Eq)

parsePreClassSkill :: PParser PreClassSkill
parsePreClassSkill = do
  n <- tag "PRECSKILL" >> manyNumbers
  classSkill <- char ',' >> parseClassSkill
  return PreClassSkill { classSkillNumber = textToInt n, .. } where
    parseClassSkill = ClassSkillType <$> (labeled "TYPE=" >> parseString)
                  <|> ClassSkillName <$> parseString

-- PREDEITY:x,y
--   x is number of required deities or pantheons
--   y is Y, N, name, or PANTHEON.name
data Deity = Worship
           | NoWorship
           | DeityName String
           | PantheonName String
              deriving (Show, Eq)

data PreDeity = PreDeity { deityNumber :: Int
                         , deities :: [Deity] }
              deriving (Show, Eq)

parsePreDeity :: PParser PreDeity
parsePreDeity = do
  n <- tag "PREDEITY" >> manyNumbers
  _ <- char ','
  deities <- parseDeity `sepBy` char ','
  return PreDeity { deityNumber = textToInt n, .. } where
    parseDeity = try (Worship <$ labeled "Y" <* notFollowedBy parseWord)
             <|> try (NoWorship <$ labeled "N" <* notFollowedBy parseWord)
             <|> PantheonName <$> (labeled "PANTHEON." >> parseStringNoCommasBrackets)
             <|> DeityName <$> parseStringNoCommasBrackets

-- PREDOMAIN:x,y,y...
--   x is number of required deity's domains
--   y is domain names or ANY
data Domain = DomainName String
            | DomainAny
              deriving (Show, Eq)

data PreDomain = PreDomain { domainNumber :: Int
                           , domains :: [Domain] }
               deriving (Show, Eq)

parsePreDomain :: PParser PreDomain
parsePreDomain = do
  n <- tag "PREDOMAIN" >> manyNumbers
  _ <- char ','
  domains <- parseDomain `sepBy` char ','
  return PreDomain { domainNumber = textToInt n, .. } where
    parseDomain = (DomainAny <$ labeled "ANY")
              <|> DomainName <$> parseStringNoCommasBrackets

-- PREDR:x,y=z,y=z...
--   x is number of conditions to be met
--   y is type of DR
--   z is number to meet or exceed
data PreDR = PreDR { preDamageResistanceNumber :: Int
                   , preDamageResistanceChecks :: [(String, Int)] }
             deriving (Show, Eq)

parsePreDR :: PParser PreDR
parsePreDR = do
  _ <- tag "PREDR"
  preDamageResistanceNumber <- parseInteger
  preDamageResistanceChecks <- parseCheckValues `sepBy` char ','
  return PreDR { .. }

-- PREEQUIP:x,y,y...
--   x is number
--   y is text, type, or wield category
data PreEquipmentType = EquipmentName String
                      | EquipmentType String
                      | WieldCategory String
                        deriving (Show, Eq)

data PreEquip = PreEquip { preEquipNumber :: Int
                         , preEquipTypes :: [PreEquipmentType] }
              deriving (Show, Eq)

_parsePreEquip :: String -> PParser PreEquip
_parsePreEquip s = do
  preEquipNumber <- tag s *> parseInteger
  _ <- char ','
  preEquipTypes <- parsePreEquipmentType `sepBy` char ','
  return PreEquip { .. } where
    parsePreEquipmentType = (labeled "WIELDCATEGORY=" >> WieldCategory <$> parseStringNoCommasBrackets)
                        <|> (labeled "TYPE=" >> EquipmentType <$> parseStringNoCommasBrackets)
                        <|> (EquipmentName <$> parseStringNoCommasBrackets)

parsePreEquip :: PParser PreEquip
parsePreEquip = _parsePreEquip "PREEQUIP"

parsePreEquipPrimary :: PParser PreEquip
parsePreEquipPrimary = _parsePreEquip "PREEQUIPPRIMARY"

parsePreEquipSecondary :: PParser PreEquip
parsePreEquipSecondary = _parsePreEquip "PREEQUIPSECONDARY"

parsePreEquipBoth :: PParser PreEquip
parsePreEquipBoth = _parsePreEquip "PREEQUIPBOTH"

parsePreEquipTwoWeapon :: PParser PreEquip
parsePreEquipTwoWeapon = _parsePreEquip "PREEQUIPTWOWEAPON"

-- PREFEAT:x,y,z,z,..
--   x is number of required feats
--   y can be CHECKMULT
--   z is feat name (or TYPE=type) ([] indicates inversion)
data Feat = FeatName String
          | FeatType String
            deriving (Show, Eq)

data PreFeat = PreFeat { featNumber :: Int
                       , feats :: [Feat]
                       , notFeats :: [Feat]
                       , countSeparately :: Bool }
               deriving (Show, Eq)

parsePreFeat :: PParser PreFeat
parsePreFeat = do
  _ <- tag "PREFEAT"
  featNumber <- parseInteger <* char ','
  countSeparately <- option False (True <$ labeled "CHECKMULT,")
  feats <- option [] $ parseFeat `sepBy` char ','
  notFeats <- option [] parseNotFeats
  return PreFeat { .. } where
    parseFeat = FeatType <$> (labeled "TYPE=" >> parseStringNoCommasBrackets)
            <|> FeatName <$> parseStringNoCommasBrackets
    parseNotFeats = char '[' *> parseFeat `sepBy` char ',' <* char ']'

-- PREGENDER:x
--   x is gender to require
data PreGenderType = Female
                   | Male
                   | Neuter
                   | NoGender
                   | Other String
                     deriving (Show, Eq)

-- number is not documented
data PreGender = PreGender { preGenderNumber :: Int
                           , preGenderType :: PreGenderType }
               deriving (Show, Eq)

parsePreGender :: PParser PreGender
parsePreGender = do
  _ <- tag "PREGENDER"
  preGenderNumber <- option 1 ((textToInt <$> manyNumbers) <* char ',')
  preGenderType <- parsePreGenderType
  return PreGender { .. } where
    -- note: case sensitive.
    parsePreGenderType = Female <$ labeled "Female"
                     <|> Female <$ labeled "F" <* notFollowedBy parseWord
                     <|> Male <$ labeled "Male"
                     <|> Male <$ labeled "M" <* notFollowedBy parseWord
                     <|> Neuter <$ labeled "Neuter"
                     <|> NoGender <$ labeled "None"
                     <|> Other <$> parseStringNoCommasBrackets

-- PREHANDSx:y
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is number
data PreHands = PreHands { preHandsOperator :: Operator
                         , preHandsNumber :: Int }
                deriving (Show, Eq)

parsePreHands :: PParser PreHands
parsePreHands = do
  op <- labeled "PREHANDS" *> choice operatorPrefixes
  _ <- char ':'
  preHandsNumber <- parseInteger
  return PreHands { preHandsOperator = convertOperator op, .. }

-- PREITEM:x,y,y,...
--   x is number of items a character must possess
--   y is text, type, or wildcard (%)
data Item = ItemName String
          | ItemType String
          | AnyItem
            deriving (Show, Eq)

data PreItem = PreItem { itemNumber :: Int
                       , items :: [Item] }
             deriving (Show, Eq)

parsePreItem :: PParser PreItem
parsePreItem = do
  n <- tag "PREITEM" >> manyNumbers
  items <- char ',' >> parseItems `sepBy` char ','
  return PreItem { itemNumber = textToInt n, .. } where
    parseItems = ItemType <$> (labeled "TYPE=" >> parseStringNoCommas)
             <|> (AnyItem <$ char '%')
             <|> ItemType <$> parseStringNoCommas

-- PRELANG:x,y,y...
--   x is number
--   y is type of language or ALL
data PreLangType = LanguageType String
                 | Language String
                 | AnyLanguage
                   deriving (Show, Eq)

data PreLang = PreLang { preLangNumber :: Int
                       , preLangTypes :: [PreLangType] }
               deriving (Show, Eq)

parsePreLang :: PParser PreLang
parsePreLang = do
  _ <- tag "PRELANG"
  preLangNumber <- parseInteger <* char ','
  preLangTypes <- parsePreLangType `sepBy` char ','
  return PreLang { .. } where
    parsePreLangType = (labeled "TYPE=" *> (LanguageType <$> parseStringNoCommas))
                   <|> (AnyLanguage <$ labeled "ALL")
                   <|> (Language <$> parseStringNoCommas)

-- PRELEGSx:y
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is number
data PreLegs = PreLegs { preLegsOperator :: Operator
                       , preLegsNumber :: Int }
               deriving (Show, Eq)

parsePreLegs :: PParser PreLegs
parsePreLegs = do
  op <- labeled "PRELEGS" *> choice operatorPrefixes
  _ <- char ':'
  preLegsNumber <- parseInteger
  return PreLegs { preLegsOperator = convertOperator op, .. }

-- PRELEVEL:x,y
--   x is MIN=formula
--   y is MAX=formula
data PreLevel = PreLevel { requiredLevelMin :: Maybe Formula
                         , requiredLevelMax :: Maybe Formula }
                deriving (Show, Eq)

parsePreLevel :: PParser PreLevel
parsePreLevel = do
  _ <- tag "PRELEVEL"
  requiredLevelMin <- tryOption (labeled "MIN=" *> parseFormula)
  requiredLevelMax <- tryOption (labeled "MAX=" *> parseFormula)
  let _ = assert (isJust requiredLevelMin || isJust requiredLevelMax)
  return PreLevel { .. }

-- PRELEVELMAX:x
--   x is number (maximum level)
parsePreLevelMax :: PParser Int
parsePreLevelMax = tag "PRELEVELMAX" *> parseInteger

-- PREMOVE:x,y=z,y=z...
--   x is minimum number movement types to pass
--   y is name of movement type
--   z is minimum number for the given movement type
data PreMove = PreMove { moveNumber :: Int
                       , moves :: [(String, Int)] }
               deriving (Show, Eq)

parsePreMove :: PParser PreMove
parsePreMove = do
  n <- tag "PREMOVE" >> manyNumbers
  moves <- char ',' >> parseMoves `sepBy` char ','
  return PreMove { moveNumber = textToInt n, .. } where
    parseMoves = do
      moveType <- parseString
      moveMinimum <- char '=' >> manyNumbers
      return (moveType, textToInt moveMinimum)

-- PREMULT:x,y,y...
--   x is number of restrictions to pass
--   y is any restriction in square brackets
data PreMult = PreMult { restrictionNumber :: Int
                       , restrictionsToPass :: [RestrictionTag] }
             deriving (Show, Eq)

parsePreMult :: PParser PreMult
parsePreMult = do
  restrictionNumber <- tag "PREMULT" *> parseInteger
  _ <- char ','
  restrictionsToPass <- parseRestrictions `sepBy` char ','
  return PreMult { .. } where
    parseRestrictions = char '[' >> parseRestriction <* char ']'

-- PREPCLEVEL:x,y
--   x is MIN=formula
--   y is MAX=formula
data PrePCLevel = PrePCLevel { requiredPCLevelMin :: Maybe Formula
                             , requiredPCLevelMax :: Maybe Formula }
                deriving (Show, Eq)

parsePrePCLevel :: PParser PrePCLevel
parsePrePCLevel = do
  _ <- tag "PREPCLEVEL"
  requiredPCLevelMin <- tryOption (labeled "MIN=" *> parseFormula)
  requiredPCLevelMax <- tryOption (labeled "MAX=" *> parseFormula)
  let _ = assert (isJust requiredPCLevelMin || isJust requiredPCLevelMax)
  return PrePCLevel { .. }

-- PREPROFWITHARMOR:x,y,y,...
-- PREPROFWITHSHIELD:x,y,y,...
--   x is number of proficiencies needed
--   y is name or type of proficiency
data ProficiencyType = ProficiencyType String
                     | ProficiencyName String
                       deriving (Show, Eq)

data PreProficiency = PreProficiency { numberOfProficiencies :: Int
                                     , proficencyTypes :: [ProficiencyType] }
                      deriving (Show, Eq)

parsePreProficency :: String -> PParser PreProficiency
parsePreProficency tagName = do
  _ <- tag tagName
  numberOfProficiencies <- parseInteger <* char ','
  proficencyTypes <- parseProficiencies `sepBy` char ','
  return PreProficiency { .. } where
    parseProficiencies = (labeled "TYPE." *> (ProficiencyType <$> parseStringNoCommasBrackets))
                     <|> (ProficiencyName <$> parseStringNoCommasBrackets)

parsePreProfWithArmor :: PParser PreProficiency
parsePreProfWithArmor = parsePreProficency "PREPROFWITHARMOR"

parsePreProfWithShield :: PParser PreProficiency
parsePreProfWithShield = parsePreProficency "PREPROFWITHSHIELD"

-- PRERACE:x,y,y...
--   x is number of racial properties
--   y is name of race, type, racetype, racesubtype
data Race = RaceName String
          | RaceType String
          | RaceTypeType String
          | RaceSubType String
            deriving (Show, Eq)

data PreRace = PreRace { raceNumber :: Int
                       , races :: [Race ] }
             deriving (Show, Eq)

parsePreRace :: PParser PreRace
parsePreRace = do
  n <- tag "PRERACE" >> manyNumbers
  races <- char ',' >> parseRaces `sepBy` char ','
  return PreRace { raceNumber = textToInt n, .. } where
    parseRaces = RaceSubType <$> (labeled "RACESUBTYPE=" *> parseStringPercentage)
             <|> RaceTypeType <$> (labeled "RACETYPE=" *> parseStringPercentage)
             <|> RaceType <$> (labeled "TYPE=" *> parseStringPercentage)
             <|> RaceName <$> parseStringPercentage
    parseStringPercentage = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#%'()~"

-- PREREGION:x
--   x: the region name.
parsePreRegion :: PParser String
parsePreRegion = tag "PREREGION" *> parseStringNoBrackets where
  parseStringNoBrackets = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()~"

-- PRERULE:x,y
--   x is number of rules required
--   y is rule name
data PreRule = PreRule { ruleNumber :: Int
                       , ruleName :: String }
               deriving (Show, Eq)

parsePreRule :: PParser PreRule
parsePreRule = do
  n <- tag "PRERULE" >> manyNumbers
  _ <- char ','
  ruleName <- parseString -- not correct but will do for now
  return PreRule { ruleNumber = textToInt n, .. }

-- PRESA:x,y,y...
--   x is number
--   y is special ability
data PreSA = PreSA { preSANumber :: Int
                   , preSATypes :: [String] }
             deriving (Show, Eq)

parsePreSA :: PParser PreSA
parsePreSA = do
  _ <- tag "PRESA"
  preSANumber <- parseInteger <* char ','
  preSATypes <- parseStringNoCommasBrackets `sepBy` char ','
  return PreSA { .. }

-- PRESIZEx:y
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is F, D, T, S, M, L, H, G, C
data PreSizeType = Fine
                 | Diminutive
                 | Tiny
                 | Small
                 | Medium
                 | Large
                 | Huge
                 | Gargantuan
                 | Colossal
                 | OtherSize String
                   deriving (Show, Eq)

data PreSize = PreSize { preSizeOperator :: Operator
                       , preSizeType :: PreSizeType }
             deriving (Show, Eq)

parsePreSize :: PParser PreSize
parsePreSize = do
  op <- labeled "PRESIZE" *> choice operatorPrefixes
  preSizeType <- parsePreSizeType
  return PreSize { preSizeOperator = convertOperator op, .. } where
    parsePreSizeType = (Fine <$ labeled "F")
                   <|> (Diminutive <$ labeled "D")
                   <|> (Tiny <$ labeled "T")
                   <|> (Small <$ labeled "S")
                   <|> (Medium <$ labeled "M")
                   <|> (Large <$ labeled "L")
                   <|> (Huge <$ labeled "H")
                   <|> (Gargantuan <$ labeled "G")
                   <|> (Colossal <$ labeled "C")
                   <|> (OtherSize <$> parseString)

-- PRESKILL:x,y=z,y=z,..
--   x is number of skills
--   y is skill name or skill type (TYPE=y)
--   z is number of skill ranks
data Skill = SkillName String
           | SkillType String
             deriving (Show, Eq)

data PreSkill = PreSkill { skillNumber :: Int
                         , skills :: [(Skill, Int)]}
                deriving (Show, Eq)

parsePreSkill :: PParser PreSkill
parsePreSkill = do
  n <- tag "PRESKILL" >> manyNumbers
  skills <- char ',' >> parseSkills `sepBy` char ','
  return PreSkill { skillNumber = textToInt n, .. } where
    parseSkills = do
      skill <- parseSkill
      val <- char '=' *> manyNumbers
      return (skill, textToInt val)
    parseSkill = SkillType <$> (labeled "TYPE=" >> parseString)
             <|> SkillName <$> parseString

-- PRESKILLTOT:x,x,...=y
--   x is skill name ($ skill type (TYPE=x)
--   y is total non-bonus skill ranks required
data PreSkillTot = PreSkillTot { skillTotals :: [Skill]
                               , skillTotalNeeded :: Int }
                   deriving (Show, Eq)

parsePreSkillTotal :: PParser PreSkillTot
parsePreSkillTotal = do
  _ <- tag "PRESKILLTOT"
  skillTotals <- parseSkills `sepBy` char ','
  n <- char '=' *> manyNumbers
  return PreSkillTot { skillTotalNeeded = textToInt n, .. } where
    parseSkills = SkillType <$> (labeled "TYPE=" >> parseStringNoCommas)
              <|> SkillName <$> parseStringNoCommas

-- PRESPELL:x,y,y,...
--   x is number of spells required
--   y is name of spell
data PreSpell = PreSpell { preSpellNumber :: Int
                         , preSpells :: [String] }
              deriving (Show, Eq)

parsePreSpell :: PParser PreSpell
parsePreSpell = do
  _ <- tag "PRESPELL"
  preSpellNumber <- parseInteger <* char ','
  preSpells <- parseStringNoCommasBrackets `sepBy` char ','
  return PreSpell { .. }

-- PRESPELLBOOK:x
--   x is YES or NO
parsePreSpellBook :: PParser Bool
parsePreSpellBook = tag "PRESPELLBOOK" *> yesOrNo

-- PRESPELLCAST:x=y,x=y,...
--   x is MEMORIZE, TYPE
--   y is Y or N (MEMORIZE only) or spell type (TYPE only)
data PreSpellCast = SpellType String
                  | MustMemorize
                  | DoNotHaveToMemorize
                    deriving (Show, Eq)

parsePreSpellCast :: PParser [PreSpellCast]
parsePreSpellCast = tag "PRESPELLCAST" *> parsePreSpellCastType `sepBy` char ',' where
  parsePreSpellCastType = (MustMemorize <$ labeled "MEMORIZE=Y")
                      <|> (DoNotHaveToMemorize <$ labeled "MEMORIZE=N")
                      <|> (labeled "TYPE=" *> (SpellType <$> parseStringNoCommasBrackets))

-- PRESPELLDESCRIPTOR:x,y=z,y=z,...
--   x is number of spells known
--   y is spell descriptor
--   z is minimum spell level
data PreSpellDescriptor = PreSpellDescriptor { preSpellDescriptorNumber :: Int
                                             , preSpellDescriptors :: [(String, Int)] }
                          deriving (Show, Eq)

parsePreSpellDescriptor :: PParser PreSpellDescriptor
parsePreSpellDescriptor = do
  _ <- tag "PRESPELLDESCRIPTOR"
  preSpellDescriptorNumber <- parseInteger <* char ','
  preSpellDescriptors <- parseCheckValues `sepBy` char ','
  return PreSpellDescriptor { .. }

-- PRESPELLSCHOOL:x,y=z,y=z,...
--   x is number of spells known
--   y is name of school of magic
--   z is minimum spell level
data PreSpellSchool = PreSpellSchool { preSpellSchoolNumber :: Int
                                     , preSpellSchools :: [(String, Int)] }
                         deriving (Show, Eq)

parsePreSpellSchool :: PParser PreSpellSchool
parsePreSpellSchool = do
  _ <- tag "PRESPELLSCHOOL"
  preSpellSchoolNumber <- parseInteger <* char ','
  preSpellSchools <- parseCheckValues `sepBy` char ','
  return PreSpellSchool { .. }

-- PRESPELLSCHOOLSUB:x,y=z,y=z
--   x is number of spells known
--   y is name of sub-school of magic
--   z is minimum spell level
data PreSpellSchoolSub = PreSpellSchoolSub { preSpellSchoolSubNumber :: Int
                                           , preSpellSchoolSubs :: [(String, Int)] }
                         deriving (Show, Eq)

parsePreSpellSchoolSub :: PParser PreSpellSchoolSub
parsePreSpellSchoolSub = do
  _ <- tag "PRESPELLSCHOOLSUB"
  preSpellSchoolSubNumber <- parseInteger <* char ','
  preSpellSchoolSubs <- parsePreSpellSchoolSubs `sepBy` char ','
  return PreSpellSchoolSub { .. } where
    parsePreSpellSchoolSubs = do
      descriptor <- parseString <* char '='
      level <- parseInteger
      return (descriptor, level)

-- PRESPELLTYPE:x,y=z,y=z,...
--   x is number of spells known
--   y is Arcane, Divine, Psionic, or ANY
--   z is spell level
data PreSpellTypeType = Arcane Int
                      | Divine Int
                      | Psionic Int
                      | AnySpellType Int
                        deriving (Show, Eq)

data PreSpellType = PreSpellType { preSpellTypeNumber :: Int
                                 , preSpellTypes :: [PreSpellTypeType] }
                         deriving (Show, Eq)

parsePreSpellType :: PParser PreSpellType
parsePreSpellType = do
  _ <- tag "PRESPELLTYPE"
  preSpellTypeNumber <- parseInteger <* char ','
  preSpellTypes <- parsePreSpellTypes `sepBy` char ','
  return PreSpellType { .. } where
    parsePreSpellTypes = AnySpellType <$> parseSpellType "Any"
                     <|> AnySpellType <$> parseSpellType "ANY" -- TODO
                     <|> Arcane <$> parseSpellType "Arcane"
                     <|> Divine <$> parseSpellType "Divine"
                     <|> Psionic <$> parseSpellType "Psionic"
    parseSpellType spell = labeled spell >> char '=' *> parseInteger

-- PRESRx:y
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is integer
data PreSR = PreSR { spellResistanceOperator :: Operator
                   , spellResistanceMin :: Int }
             deriving (Show, Eq)

parsePreSR :: PParser PreSR
parsePreSR = do
  op <- labeled "PRESR" >> choice operatorPrefixes
  spellResistanceMin <- char ':' >> parseInteger
  return PreSR { spellResistanceOperator = convertOperator op, .. }

-- PRESTAT:x,y=z,y=z,..
--   x is number
--   y is stats abbrevation
--   z is number
data PreStat = PreStat { statNumber :: Int
                       , statChecks :: [(String, Int)] }
             deriving (Show, Eq)

parsePreStat :: PParser PreStat
parsePreStat = do
  _ <- tag "PRESTAT"
  statNumber <- parseInteger
  _ <- char ','
  statChecks <- parseStatCheck `sepBy` char ','
  return PreStat { .. } where
    parseStatCheck = do
      stat <- parseStringNoCommas <* char '='
      num <- parseInteger
      return (stat, num)

-- PRESUBCLASS:x,y,y...
--   x is number
--   y is subclass name
data PreSubClass = PreSubClass { preSubClassNumber :: Int
                               , preSubClassNames :: [String] }
                 deriving (Show, Eq)

parsePreSubClass :: PParser PreSubClass
parsePreSubClass = do
  _ <- tag "PRESUBCLASS"
  preSubClassNumber <- parseInteger
  _ <- char ','
  preSubClassNames <- parseStringNoCommasBrackets `sepBy` char ','
  return PreSubClass { .. }

-- PRETEMPLATE:x,y,y...
--   x is number
--   y is template name or type
data PreTemplateType = TemplateName String
                     | TemplateType String
                       deriving (Show, Eq)

data PreTemplate = PreTemplate { preTemplateNumber :: Int
                               , preTemplateNames :: [PreTemplateType] }
                   deriving (Show, Eq)

parsePreTemplate :: PParser PreTemplate
parsePreTemplate = do
  _ <- tag "PRETEMPLATE"
  preTemplateNumber <- parseInteger
  _ <- char ','
  preTemplateNames <- parsePreTemplateTypes `sepBy` char ','
  return PreTemplate { .. } where
    parsePreTemplateTypes = (labeled "TYPE=" *> (TemplateType <$> parseStringNoCommasBrackets))
                        <|> TemplateName <$> parseStringNoCommasBrackets

-- PRETEXT:x
--   where x is explanation of requirement
parsePreText :: PParser String
parsePreText = tag "PRETEXT" *> restOfTag

-- PRETOTALAB:x
--   x is integer
parsePreTotalAB :: PParser Int
parsePreTotalAB = tag "PRETOTALAB" *> parseInteger

-- PRETYPE:x,y,y...
--   x is number
--   y is type requirement
data PreType = PreType { preTypeNumber :: Int
                       , preTypeRequirements :: [String] }
             deriving (Show, Eq)

-- NB: account for e.g., PRETYPE:1,EQMOD=BROKEA; furthermore, this
-- should correspond to the Equipment type tag.
parsePreType :: PParser PreType
parsePreType = do
  _ <- tag "PRETYPE"
  preTypeNumber <- parseInteger
  _ <- char ','
  preTypeRequirements <- parseStringEquals `sepBy` char ','
  return PreType { .. } where
    parseStringEquals = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()~="

-- PREVARx:y,z
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is text (must be in DEFINE: or BONUS:VAR)
--   z is number to be compared to
--
-- NB: documentation does not seem quite right. each y,z parameter is
-- probably a formula. for now, we read in unknown variables as Text.
data PreVarType = PreVarFormula Formula
                | PreVarText String
                  deriving (Show, Eq)

data PreVar = PreVar { operator :: Operator
                     , variables :: [PreVarType] }
              deriving (Show, Eq)

parsePreVar :: PParser PreVar
parsePreVar = do
  op <- labeled "PREVAR" >> choice operatorPrefixes
  variables <- char ':' >> parsePreVarType `sepBy` char ','
  return PreVar { operator = convertOperator op, .. } where
    parsePreVarType = PreVarFormula <$> try parseFormula
                  <|> PreVarText <$> parseStringNoCommasBrackets

-- PREVISION:x,y=z,y=z
--   x is number of matching vision types
--   y is vision type
--   z is number or ANY
data PreVisionType = Vision String Int
                   | AnyVision String
                     deriving (Show, Eq)

data PreVision = PreVision { preVisionNumber :: Int
                           , preVisionTypes :: [PreVisionType] }
               deriving (Show, Eq)

parsePreVision :: PParser PreVision
parsePreVision = do
  _ <- tag "PREVISION"
  preVisionNumber <- parseInteger <* char ','
  preVisionTypes <- parsePreVisionTypes `sepBy` char ','
  return PreVision { .. } where
    parsePreVisionTypes = do
      check <- parseString <* char '='
      parseVisionValue check
    parseVisionValue str = AnyVision str <$ labeled "ANY"
                       <|> (Vision str <$> parseInteger)

-- PREWEAPONPROF:x,y,y...
--   x is number of matching proficiencies
--   y is name or type or DEITYWEAPON
data PreWeaponProfType = PreWeaponName String
                       | PreWeaponType String
                       | DeityWeapon
                         deriving (Show, Eq)

data PreWeaponProf = PreWeaponProf { preWeaponProfNumber :: Int
                                   , preWeaponProfType :: [PreWeaponProfType] }
                   deriving (Show, Eq)

parsePreWeaponProf :: PParser PreWeaponProf
parsePreWeaponProf = do
  _ <- tag "PREWEAPONPROF"
  preWeaponProfNumber <- parseInteger
  _ <- char ','
  preWeaponProfType <- parseWeaponProfType `sepBy` char ','
  return PreWeaponProf { .. } where
    parseWeaponProfType = (DeityWeapon <$ labeled "DEITYWEAPON")
                      <|> (labeled "TYPE=" *> (PreWeaponType <$> parseStringNoCommasBrackets))
                      <|> (PreWeaponName <$> parseStringNoCommasBrackets)

-- PREWIELD:x,y
--   x is number
--   y is type
data PreWieldType = Light
                  | OneHanded
                  | TwoHanded
                    deriving (Show, Eq)

data PreWield = PreWield { preWieldNumber :: Int
                         , preWieldType :: PreWieldType }
                deriving (Show, Eq)

parsePreWield :: PParser PreWield
parsePreWield = do
  _ <- tag "PREWIELD"
  preWieldNumber <- parseInteger
  _ <- char ','
  preWieldType <- parsePreWieldType
  return PreWield { .. } where
    parsePreWieldType = (Light <$ labeled "Light")
                    <|> (OneHanded <$ labeled "OneHanded")
                    <|> (TwoHanded <$ labeled "TwoHanded")

parsePossibleRestriction :: PParser RestrictionTag
parsePossibleRestriction = PreVarRestriction <$> parsePreVar
                       <|> PreClassSkillRestriction <$> parsePreClassSkill
                       <|> PreClassRestriction <$> parsePreClass
                       <|> PreDeityRestriction <$> parsePreDeity
                       <|> PreDomainRestriction <$> parsePreDomain
                       <|> PreAbilityRestriction <$> parsePreAbility
                       <|> PreFeatRestriction <$> parsePreFeat
                       <|> PreItemRestriction <$> parsePreItem
                       <|> PreRaceRestriction <$> parsePreRace
                       <|> PreMoveRestriction <$> parsePreMove
                       <|> PreStatRestriction <$> parsePreStat
                       <|> PreTypeRestriction <$> parsePreType
                       <|> PreSizeRestriction <$> parsePreSize
                       <|> PreLegsRestriction <$> parsePreLegs
                       <|> PreLangRestriction <$> parsePreLang
                       <|> PreSpecialAbilityRestriction <$> parsePreSA
                       <|> PreSpellResistanceRestriction <$> parsePreSR
                       <|> PreDamageResistanceRestriction <$> parsePreDR
                       <|> PreHandsRestriction <$> parsePreHands
                       <|> PreRegionRestriction <$> parsePreRegion
                       <|> PreSubClassRestriction <$> parsePreSubClass
                       <|> PreAttackRestriction <$> parsePreAttack
                       <|> PreRuleRestriction <$> parsePreRule
                       <|> PreCheckBaseRestriction <$> parsePreCheckBase
                       <|> PreAlignRestriction <$> parsePreAlign
                       <|> PreEquipRestriction <$> parsePreEquip
                       <|> PreEquipBothRestriction <$> parsePreEquipBoth
                       <|> PreEquipTwoWeaponRestriction <$> parsePreEquipTwoWeapon
                       <|> PreEquipPrimaryRestriction <$> parsePreEquipPrimary
                       <|> PreEquipSecondaryRestriction <$> parsePreEquipSecondary
                       <|> PreLevelRestriction <$> parsePreLevel
                       <|> PreLevelMaxRestriction <$> parsePreLevelMax
                       <|> PreSpellRestriction <$> parsePreSpell
                       <|> PreSpellBookRestriction <$> parsePreSpellBook
                       <|> PreSpellCastRestriction <$> parsePreSpellCast
                       <|> PreSpellDescriptorRestriction <$> parsePreSpellDescriptor
                       <|> PreSpellSchoolRestriction <$> parsePreSpellSchool
                       <|> PreSpellSchoolSubRestriction <$> parsePreSpellSchoolSub
                       <|> PreSpellTypeRestriction <$> parsePreSpellType
                       <|> PreTextRestriction <$> parsePreText
                       <|> PreTemplateRestriction <$> parsePreTemplate
                       <|> PreTotalAttackBonus <$> parsePreTotalAB
                       <|> PreProficencyArmorRestriction <$> parsePreProfWithArmor
                       <|> PreProficencyShieldRestriction <$> parsePreProfWithShield
                       <|> PrePCLevelRestriction <$> parsePrePCLevel
                       <|> PreVisionRestriction <$> parsePreVision
                       <|> PreWeaponProfRestriction <$> parsePreWeaponProf
                       <|> PreWieldRestriction <$> parsePreWield
                       <|> PreSkillTotalRestriction <$> parsePreSkillTotal
                       <|> PreSkillRestriction <$> parsePreSkill
                       <|> PreGenderRestriction <$> parsePreGender
                       <|> PreMultipleRestriction <$> parsePreMult

parseRestriction :: PParser RestrictionTag
parseRestriction = parseInvertedRestriction parsePossibleRestriction
                                        <|> parsePossibleRestriction where
  parseInvertedRestriction p = char '!' >> Invert <$> p

-- for chained restrictions (e.g., BONUS tags)
parseAdditionalRestrictions :: PParser [RestrictionTag]
parseAdditionalRestrictions = many $ try restrictions where
  restrictions = char '|' >> parseRestriction
