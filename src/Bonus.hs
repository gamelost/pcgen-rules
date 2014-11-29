{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import Text.Parsec.Char (char, space, string, satisfy)
import Text.Parsec.Combinator (sepBy, option, many1)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import JEPFormula
import Common

data Bonus = BonusSkill Skill
           | BonusSkillChoice Int
           | BonusSkillRank SkillRank
           | BonusVariable BonusVar
           | BonusWeaponProficency BonusWeaponProf
           | BonusWeaponProperty BonusWeaponProp
           | BonusAbilityPool BonusAbility
           | BonusCasterLevel CasterLevel
           | BonusCombat Combat
           | BonusCheck Checks
           | BonusDamageReduction BonusDR
           | BonusModifyEquipmentWeight BonusEquipmentWeight
           | BonusModifyEquipmentPenalty BonusEquipmentPenalty
           | BonusModifyEquipmentRange BonusEquipmentRange
           | BonusHitPoint BonusHP
           | BonusItemCost ItemCost
           | BonusSizeModifier Formula
           | BonusAddSave BonusSave
           | BonusAddSituation BonusSituation
           | BonusDifficultyClass BonusDC
           | BonusVision BonusVisionData
           | BonusSlotItems BonusSlots
           | BonusSpellCasting BonusSpellCast
           | BonusSpellCastingMultiple BonusSpellCastMult
           | BonusSpellKnown BonusSpellCastMult
           | BonusSkillPoints Formula
           | BonusPostMoveAddition BonusPostMoveAdd
           | BonusMiscellany BonusMisc
           | BonusMovement BonusMove
           | BonusMovementMultiplier BonusMoveMultiplier
           | BonusDescription String
           | BonusCharacterStat BonusStat
           | BonusCharacterStatChoice ()
           | BonusUnarmedDamage UnarmedDamage
           | TemporaryBonus TempBonus
             deriving (Show, Eq)

data BonusTag = BonusTag { bonus :: Bonus
                         , bonusType :: Maybe (String, Bool)
                         , bonusRestrictions :: [RestrictionTag] }
                deriving (Show, Eq)

bonusTag :: String -> PParser String
bonusTag t = labeled $ t ++ "|"

-- we have far more bonus types, but for now, stick with a simple (Text, Bool)
parseBonusType :: PParser (String, Bool)
parseBonusType = do
  bonusType <- types *> parseString
  let testForStack = stripSuffix ".STACK" bonusType
  return (fromMaybe bonusType testForStack, isJust testForStack) where
    types = labeled "|TYPE="
        <|> labeled "|SKILLTYPE="

-- bonus types can be found either before or after restrictions
parseBonusRestrictionsAndType :: PParser ([RestrictionTag], Maybe (String, Bool))
parseBonusRestrictionsAndType = do
  type1 <- tryOption parseBonusType
  restrictions <- tryOption parseAdditionalRestrictions
  type2 <- tryOption parseBonusType
  -- make sure we didn't parse bonus TYPEs both times!
  let _ = assert (isJust type1 && isJust type2)
  let bonusType = type1 <|> type2
  let bonusRestrictions = fromMaybe [] restrictions
  return (bonusRestrictions, bonusType)

-- BONUS:ABILITYPOOL|x|y
--   x is ability category
--   y is formula added to pool (not number!)
data BonusAbility = BonusAbility { abilityCategory :: String
                                 , abilityPoolFormula :: Formula }
                  deriving (Show, Eq)

parseBonusAbilityPool :: PParser BonusAbility
parseBonusAbilityPool = do
  _ <- bonusTag "ABILITYPOOL"
  abilityCategory <- parseTill '|'
  abilityPoolFormula <- parseFormula
  return BonusAbility { .. }

-- BONUS:CASTERLEVEL|SUBSCHOOL.Creation|1|PRERULE:1,SYS_DOMAIN

-- BONUS:CASTERLEVEL|x|y
--   x is class, spell, domain, race, school, spell, subschool, or class type
--   y is number, variable, formula
data CasterLevelType = CLAllSpells
                     | CLDescriptor String
                     | CLDomain String
                     | CLName String
                     | CLRace String
                     | CLSchool String
                     | CLSpell String
                     | CLSubSchool String
                     | CLType String
                       deriving (Show, Eq)

data CasterLevel = CasterLevel { casterLevel :: CasterLevelType
                               , casterFormula :: Formula }
                 deriving (Show, Eq)

parseBonusCasterLevel :: PParser CasterLevel
parseBonusCasterLevel = do
  _ <- bonusTag "CASTERLEVEL"
  casterLevel <- parseCasterLevelType
  casterFormula <- parseFormula
  return CasterLevel { .. } where
    parseCasterLevelType :: PParser CasterLevelType
    parseCasterLevelType = (CLAllSpells <$ labeled "ALLSPELLS")
                       <|> (labeled "DESCRIPTOR." >> CLDescriptor <$> parseRest)
                       <|> (labeled "DOMAIN." >> CLDomain <$> parseRest)
                       <|> (labeled "RACE." >> CLRace <$> parseRest)
                       <|> (labeled "SCHOOL." >> CLSchool <$> parseRest)
                       <|> (labeled "SPELL." >> CLSpell <$> parseRest)
                       <|> (labeled "SUBSCHOOL." >> CLSubSchool <$> parseRest)
                       <|> (labeled "TYPE." >> CLType <$> parseRest)
                       <|> CLName <$> parseRest
    parseRest = parseTill '|'

-- BONUS:CHECKS|x,x|y
--   x is ALL, BASE.check name, or check name
--   y is number, variable or formula
data CheckName = CheckAll
               | CheckBase String
               | CheckName String
                 deriving (Show, Eq)

data Checks = Checks { checks :: [CheckName]
                     , checkFormula :: Formula }
              deriving (Show, Eq)

parseBonusCheck :: PParser Checks
parseBonusCheck = do
  _ <- bonusTag "CHECKS"
  checks <- parseChecks `sepBy` char ','
  checkFormula <- char '|' *> parseFormula
  return Checks { .. } where
    parseChecks :: PParser CheckName
    parseChecks = (CheckAll <$ labeled "ALL")
              <|> (labeled "BASE." >> CheckBase <$> parseStringNoCommas)
              <|> CheckName <$> parseStringNoCommas

-- BONUS:COMBAT|x|y
--   x is combat bonus type
--   y is formula
data BonusCombatCategory = BC_AC
                         | BC_ATTACKS
                         | BC_BAB
                         | BC_BASEAB
                         | BC_DAMAGE
                         | BC_DAMAGE_TYPE String
                         | BC_DAMAGEMULT_OFFHAND -- 0
                         | BC_DAMAGEMULT_PRIMARY -- 1
                         | BC_DAMAGEMULT_TWO_HAND -- 2
                         | BC_DAMAGESIZE
                         | BC_DAMAGE_SHORTRANGE
                         | BC_EPICAB
                         | BC_INITIATIVE
                         | BC_HP -- undocumented
                         | BC_REACH
                         | BC_RANGEPENALTY
                         | BC_SECONDARYATTACKS
                         | BC_SECONDARYDAMAGE
                         | BC_TOHIT
                         | BC_TOHIT_TYPE String
                         | BC_TOHIT_PRIMARY
                         | BC_TOHIT_SECONDARY
                         | BC_TOHIT_SHORTRANGE
                           deriving (Show, Eq)

data Combat = Combat { combatCategories :: [BonusCombatCategory]
                     , combatFormula :: Formula }
              deriving (Show, Eq)

parseBonusCombat :: PParser Combat
parseBonusCombat = do
  _ <- bonusTag "COMBAT"
  combatCategories <- parseCombatType `sepBy` char ','
  -- aeg_gods_equip_magic.lst does not have a formula, default to 1
  combatFormula <- option (Number 1) (try (char '|' *> parseFormula))
  return Combat { .. } where
    parseCombatType = try (BC_AC <$ labeled "AC")
                  <|> try (BC_ATTACKS <$ labeled "ATTACKS")
                  <|> try (BC_BAB <$ labeled "BAB")
                  <|> try (BC_BASEAB <$ labeled "BASEAB")
                  <|> try (BC_DAMAGEMULT_OFFHAND <$ labeled "DAMAGEMULT:0")
                  <|> try (BC_DAMAGEMULT_PRIMARY <$ labeled "DAMAGEMULT:1")
                  <|> try (BC_DAMAGEMULT_TWO_HAND <$ labeled "DAMAGEMULT:2")
                  <|> try (BC_DAMAGESIZE <$ labeled "DAMAGESIZE")
                  <|> try (BC_DAMAGE_SHORTRANGE <$ labeled "DAMAGE-SHORTRANGE")
                  <|> try (labeled "DAMAGE." >> BC_DAMAGE_TYPE <$> parseString)
                  <|> try (BC_DAMAGE <$ labeled "DAMAGE")
                  <|> try (BC_EPICAB <$ labeled "EPICAB")
                  <|> try (BC_INITIATIVE <$ labeled "INITIATIVE")
                  <|> try (BC_HP <$ labeled "HP")
                  <|> try (BC_REACH <$ labeled "REACH")
                  <|> try (BC_RANGEPENALTY <$ labeled "RANGEPENALTY")
                  <|> try (BC_SECONDARYATTACKS <$ labeled "SECONDARYATTACKS")
                  <|> try (BC_SECONDARYDAMAGE <$ labeled "SECONDARYDAMAGE")
                  <|> try (labeled "TOHIT." >> BC_TOHIT_TYPE <$> parseString)
                  <|> try (BC_TOHIT_PRIMARY <$ labeled "TOHIT-PRIMARY")
                  <|> try (BC_TOHIT_SECONDARY <$ labeled "TOHIT-SECONDARY")
                  <|> try (BC_TOHIT_SHORTRANGE <$ labeled "TOHIT-SHORTRANGE")
                  <|> (BC_TOHIT <$ labeled "TOHIT")

-- BONUS:DC|x|y
--   x is spell, class, domain
--   y is formula
data BonusDCType = SpellName String
                 | SpellDescriptor String
                 | AllSpells
                 | ClassName String
                 | DomainName String
                 | FeatBonus
                 | SchoolName String
                 | SubSchoolName String
                 | SpellType String
                   deriving (Show, Eq)

data BonusDC = BonusDC { difficultyFocus :: BonusDCType
                       , difficultyFormula :: Formula }
             deriving (Show, Eq)

parseBonusDC :: PParser BonusDC
parseBonusDC = do
  _ <- bonusTag "DC"
  difficultyFocus <- parseDifficultyFocus
  difficultyFormula <- char '|' *> parseFormula
  return BonusDC { .. } where
    parseDifficultyFocus = (AllSpells <$ labeled "ALLSPELLS")
                       <|> (FeatBonus <$ labeled "FEATBONUS")
                       <|> (labeled "SPELL." >> SpellName <$> parseString)
                       <|> (labeled "CLASS." >> ClassName <$> parseString)
                       <|> (labeled "DOMAIN." >> DomainName <$> parseString)
                       <|> (labeled "SCHOOL." >> SchoolName <$> parseString)
                       <|> (labeled "SUBSCHOOL." >> SubSchoolName <$> parseString)
                       <|> (labeled "DESCRIPTOR." >> SpellDescriptor <$> parseString)
                       <|> (labeled "TYPE." >> SpellType <$> parseString)

-- BONUS:DR|x|y
--   x is damage reduction type
--   y is formula
data BonusDR = BonusDR { damageReductionType :: String
                       , damageReductionFormula :: Formula }
             deriving (Show, Eq)

parseBonusDR :: PParser BonusDR
parseBonusDR = do
  _ <- bonusTag "DR"
  damageReductionType <- parseTill '|' -- TODO need to implement DR tag
  damageReductionFormula <- parseFormula
  return BonusDR { .. }

-- BONUS:EQM|x|y|z
--   x is HANDS, WEIGHTADD, WEIGHTDIV, WEIGHTMULT
--   y is formula
--   z is type (optional)
data BonusEquipmentWeightType = Hands
                              | AddWeight
                              | DivideWeight
                              | MultiplyWeight
                                deriving (Show, Eq)

data BonusEquipmentWeight = BonusEquipmentWeight { bonusEquipmentWeightType :: BonusEquipmentWeightType
                                                 , bonusEquipmentWeightFormula :: Formula
                                                 , bonusEquipmentWeightBonusType :: Maybe String }
                            deriving (Show, Eq)

parseBonusEquipmentWeight :: PParser BonusEquipmentWeight
parseBonusEquipmentWeight = do
  _ <- bonusTag "EQM"
  bonusEquipmentWeightType <- parseBonusEquipmentWeightType
  bonusEquipmentWeightFormula <- char '|' *> parseFormula
  bonusEquipmentWeightBonusType <- tryOption parseBonusWeightType
  return BonusEquipmentWeight { .. } where
    parseBonusEquipmentWeightType = (Hands <$ labeled "HANDS")
                                <|> (AddWeight <$ labeled "WEIGHTADD")
                                <|> (DivideWeight <$ labeled "WEIGHTDIV")
                                <|> (MultiplyWeight <$ labeled "WEIGHTMULT")
    parseBonusWeightType = labeled "|TYPE=" *> parseString

-- BONUS:EQMWEAPON|x|y|z
--   x is CRITRANGEADD, CRITRANGEDOUBLE, DAMAGESIZE, RANGEADD, RANGEMULT
--   y is formula
--   z is type (optional)
data BonusEquipmentRangeType = AddCriticalRange
                             | DoubleCriticalRange
                             | DamageSize
                             | AddRange
                             | MultiplyRange
                               deriving (Show, Eq)

data BonusEquipmentRange = BonusEquipmentRange { bonusEquipmentRangeType :: BonusEquipmentRangeType
                                               , bonusEquipmentRangeFormula :: Formula
                                               , bonusEquipmentRangeBonusType :: Maybe String }
                           deriving (Show, Eq)

parseBonusEquipmentRange :: PParser BonusEquipmentRange
parseBonusEquipmentRange = do
  _ <- bonusTag "EQMWEAPON"
  bonusEquipmentRangeType <- parseBonusEquipmentRangeType
  bonusEquipmentRangeFormula <- char '|' *> parseFormula
  bonusEquipmentRangeBonusType <- tryOption parseBonusRangeType
  return BonusEquipmentRange { .. } where
    parseBonusEquipmentRangeType = (AddCriticalRange <$ labeled "CRITRANGEADD")
                               <|> (DoubleCriticalRange <$ labeled "CRITRANGEDOUBLE")
                               <|> (DamageSize <$ labeled "DAMAGESIZE")
                               <|> (AddRange <$ labeled "RANGEADD")
                               <|> (MultiplyRange <$ labeled "RANGEMULT")
    parseBonusRangeType = labeled "|TYPE=" *> parseString

-- BONUS:EQMARMOR|x|y|z
--   x is ACCHECK, EDR, MAXDEX, SPELLFAILURE
--   y is formula
--   z is type (optional)
data BonusEquipmentPenaltyType = ACCheckPenalty
                               | EffectiveDamageResistance
                               | MaxDexerity
                               | SpellFailurePenalty
                                 deriving (Show, Eq)

data BonusEquipmentPenalty = BonusEquipmentPenalty { bonusEquipmentPenaltyType :: BonusEquipmentPenaltyType
                                                   , bonusEquipmentPenaltyFormula :: Formula
                                                   , bonusEquipmentPenaltyBonusType :: Maybe String }
                             deriving (Show, Eq)

parseBonusEquipmentPenalty :: PParser BonusEquipmentPenalty
parseBonusEquipmentPenalty = do
  _ <- bonusTag "EQMARMOR"
  bonusEquipmentPenaltyType <- parseBonusEquipmentPenaltyType
  bonusEquipmentPenaltyFormula <- char '|' *> parseFormula
  bonusEquipmentPenaltyBonusType <- tryOption parseBonusPenaltyType
  return BonusEquipmentPenalty { .. } where
    parseBonusEquipmentPenaltyType = (ACCheckPenalty <$ labeled "ACCHECK")
                                 <|> (EffectiveDamageResistance <$ labeled "EDR")
                                 <|> (MaxDexerity <$ labeled "MAXDEX")
                                 <|> (SpellFailurePenalty <$ labeled "SPELLFAILURE")
    parseBonusPenaltyType = labeled "|TYPE=" *> parseString

-- BONUS:HP|x|y
--   x is ALTHP or CURRENTMAX
--   y is formula
data BonusHPType = AlternateHP
                 | CurrentMax
                   deriving (Show, Eq)

data BonusHP = BonusHP { bonusHPType :: BonusHPType
                       , bonusHPFormula :: Formula }
             deriving (Show, Eq)

parseBonusHP :: PParser BonusHP
parseBonusHP = do
  _ <- bonusTag "HP"
  bonusHPType <- parseBonusHPType
  bonusHPFormula <- char '|' *> parseFormula
  return BonusHP { .. } where
    parseBonusHPType = (AlternateHP <$ labeled "ALTHP")
                   <|> (CurrentMax <$ labeled "CURRENTMAX")

-- BONUS:ITEMCOST|TYPE.x.x|y
--   x is item type
--   y is formula
data ItemCost = ItemCost { itemCostType :: [String]
                         , itemCostFormula :: Formula }
              deriving (Show, Eq)

parseBonusItemCost :: PParser ItemCost
parseBonusItemCost = do
  _ <- bonusTag "ITEMCOST"
  itemCostTypes <- parseItemCostType `sepBy` char ','
  itemCostFormula <- char '|' *> parseFormula
  let itemCostType = concat itemCostTypes
  return ItemCost { .. } where
    parseItemCostType = labeled "TYPE." >> parseStringNoPeriods `sepBy` char '.'
    parseStringNoPeriods = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+/:?!%#'()[]~"

-- BONUS:MISC|x|y
--   x is ACCHECK, CR, MAXDEX, SPELLFAILURE, or SR
--   y is formula
data BonusMiscType = ACCheck
                   | CR
                   | MaxDex
                   | SpellFailure
                   | SR
                     deriving (Show, Eq)

data BonusMisc = BonusMisc { bonusMiscType :: BonusMiscType
                           , bonusMiscFormula :: Formula }
               deriving (Show, Eq)

parseBonusMisc :: PParser BonusMisc
parseBonusMisc = do
  _ <- bonusTag "MISC"
  bonusMiscType <- parseBonusMiscType
  bonusMiscFormula <- char '|' *> parseFormula
  return BonusMisc { .. } where
    parseBonusMiscType = (ACCheck <$ labeled "ACCHECK")
                     <|> (CR <$ labeled "CR")
                     <|> (MaxDex <$ labeled "MAXDEX")
                     <|> (SpellFailure <$ labeled "SPELLFAILURE")
                     <|> (SR <$ labeled "SR")

-- BONUS:MOVEADD|x|y
--   x is movement type or all
--   y is formula
data BonusMoveType = Movement String
                   | AllMovement
                     deriving (Show, Eq)

data BonusMove = BonusMove { bonusMove :: BonusMoveType
                           , bonusMoveFormula :: Formula }
               deriving (Show, Eq)

parseBonusMoveAdd :: PParser BonusMove
parseBonusMoveAdd = do
  _ <- bonusTag "MOVEADD"
  bonusMove <- parseBonusMoveType
  bonusMoveFormula <- char '|' *> parseFormula
  return BonusMove { .. } where
    parseBonusMoveType = (AllMovement <$ labeled "TYPE.All")
                     <|> (labeled "TYPE." >> Movement <$> parseString)

-- BONUS:MOVEMULT|x...|y
--   x is TYPE.movement or TYPE.All
--   y is formula
data BonusMoveMultiplierTypes = BonusMoveMultiplierType String
                              | BonusMoveMultiplierAll
                                deriving (Show, Eq)

data BonusMoveMultiplier = BonusMoveMultiplier { bonusMoveMultiplierTypes :: [BonusMoveMultiplierTypes]
                                               , bonusMoveMultiplierFormula :: Formula }
                         deriving (Show, Eq)

parseBonusMoveMultiplier :: PParser BonusMoveMultiplier
parseBonusMoveMultiplier = do
  _ <- bonusTag "MOVEMULT"
  bonusMoveMultiplierTypes <- parseBonusMoveMultiplierTypes `sepBy` char ','
  bonusMoveMultiplierFormula <- char '|' *> parseFormula
  return BonusMoveMultiplier { .. } where
    parseBonusMoveMultiplierTypes = (BonusMoveMultiplierAll <$ labeled "TYPE.All")
                                <|> (labeled "TYPE." >> BonusMoveMultiplierType <$> parseStringNoCommas)

-- BONUS:POSTMOVEADD|x|y
--   x is movement type or all
--   y is formula
data BonusPostMoveAddType = MovementPostType String
                          | AllPostMovement
                            deriving (Show, Eq)

data BonusPostMoveAdd = BonusPostMoveAdd { bonusPostMoveAddType :: BonusPostMoveAddType
                                         , bonusPostMoveAddFormula :: Formula }
                      deriving (Show, Eq)

parseBonusPostMoveAdd :: PParser BonusPostMoveAdd
parseBonusPostMoveAdd = do
  _ <- bonusTag "POSTMOVEADD"
  bonusPostMoveAddType <- parseBonusPostMoveAddType
  bonusPostMoveAddFormula <- char '|' *> parseFormula
  return BonusPostMoveAdd { .. } where
    parseBonusPostMoveAddType = (AllPostMovement <$ labeled "TYPE.All")
                            <|> (labeled "TYPE." *> (MovementPostType <$> parseString))

-- BONUS:SAVE|x,x...|y
--   x is save name or BASE.text or ALL
--   y is formula
data BonusSaveType = SaveName String
                   | BaseSaveName String
                   | SaveAll
                     deriving (Show, Eq)

data BonusSave = BonusSave { bonusSaveTypes :: [BonusSaveType]
                           , bonusSaveFormula :: Formula }
               deriving (Show, Eq)

parseBonusSave :: PParser BonusSave
parseBonusSave = do
  _ <- bonusTag "SAVE"
  bonusSaveTypes <- parseBonusSaveTypes `sepBy` char ','
  _ <- char '|'
  bonusSaveFormula <- parseFormula
  return BonusSave { .. } where
    parseBonusSaveTypes = (labeled "BASE." *> (BaseSaveName <$> parseStringNoPeriods))
                      <|> (SaveAll <$ labeled "ALL")
                      <|> (SaveName <$> parseStringNoPeriods)
    parseStringNoPeriods = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+/:?!%#'()[]~"

-- BONUS:SITUATION|x=y,x=y|z
--   x is skill name
--   y is situation
--   z is formula
data BonusSituation = BonusSituation { bonusSituations :: [(String, String)]
                                     , bonusSituationFormula :: Formula }
                    deriving (Show, Eq)

parseBonusSituation :: PParser BonusSituation
parseBonusSituation = do
  _ <- bonusTag "SITUATION"
  bonusSituations <- parseBonusSituations `sepBy` char ','
  _ <- char '|'
  bonusSituationFormula <- parseFormula
  return BonusSituation { .. } where
    parseBonusSituations = do
      skill <- parseStringNoCommas <* char '='
      situation <- parseStringNoCommas
      return (skill, situation)

-- BONUS:SIZEMOD|NUMBER|x
--   x is formula
parseBonusSizeMod :: PParser Formula
parseBonusSizeMod = bonusTag "SIZEMOD|NUMBER" *> parseFormula

-- BONUS:SKILL:x,x,...|y
--   x is LIST, ALL, skill name, stat name (STAT.x), skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkill = List
                  | All
                  | BonusSkillName String
                  | BonusSkillType String
                  | StatName String
                    deriving (Show, Eq)

data SkillFormulaType = SkillFormula Formula
                      | SkillText String
                        deriving (Show, Eq)

data Skill = Skill { bonusToSkills :: [BonusToSkill]
                   , skillFormula :: SkillFormulaType }
             deriving (Show, Eq)

parseBonusSkill :: PParser Skill
parseBonusSkill = do
  _ <- bonusTag "SKILL"
  bonusToSkills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  return Skill { .. } where
    parseBonusSkills = parseList
                   <|> parseAll
                   <|> parseSkillType
                   <|> parseStatName
                   <|> parseSkillName
    parseList = List <$ labeled "LIST"
    parseAll = All <$ labeled "ALL"
    parseSkillType = labeled "TYPE=" >> (BonusSkillType <$> parseStringNoCommas)
    parseStatName = labeled "STAT." >> (StatName <$> parseStringNoCommas)
    -- magical_treasures_equip_artifacts.lst has an asterisk.
    parseSkillName = BonusSkillName <$> parseStringNoCommasAsterisk
    parseSkillFormulaType = SkillFormula <$> try parseFormula
                        <|> SkillText <$> parseStringNoCommas
    parseStringNoCommasAsterisk = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~*"

-- BONUS:SKILL|%CHOICE
-- TODO: define.
parseBonusSkillChoice :: PParser Int
parseBonusSkillChoice = do
  _ <- labeled "SKILL|%CHOICE"
  option 1 (try $ char '|' *> (textToInt <$> manyNumbers))

-- BONUS:SKILLRANK|x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName String
                      | SkillRankType String
                        deriving (Show, Eq)

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: SkillFormulaType }
                 deriving (Show, Eq)

parseBonusSkillRank :: PParser SkillRank
parseBonusSkillRank = do
  _ <- bonusTag "SKILLRANK"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseSkillFormulaType
  return SkillRank { .. } where
    parseBonusSkillRanks = many space >> (parseSkillType <|> parseSkillName)
    parseSkillType = labeled "TYPE=" >> (SkillRankType <$> parseStringNoCommas)
    parseSkillName = SkillRankName <$> parseStringNoCommas
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> (labeled "SKILLRANK=" >> parseStringNoCommas)

-- BONUS:SKILLPOINTS|NUMBER|x
--   x is formula
parseBonusSkillPoints :: PParser Formula
parseBonusSkillPoints = bonusTag "SKILLPOINTS|NUMBER" *> parseFormula

-- BONUS:SLOTS|x,y
--   x is slot type or LIST
--   y is formula
data BonusSlotType = SlotList
                   | SlotType String
                     deriving (Show, Eq)

data BonusSlots = BonusSlots { bonusSlotType :: BonusSlotType
                             , bonusSlotFormula :: Formula }
                  deriving (Show, Eq)

parseBonusSlots :: PParser BonusSlots
parseBonusSlots = do
  _ <- bonusTag "SLOTS"
  bonusSlotType <- parseBonusSlotType
  bonusSlotFormula <- char '|' *> parseFormula
  return BonusSlots { .. } where
    parseBonusSlotType = (SlotList <$ labeled "LIST")
                     <|> (SlotType <$> parseString)

-- BONUS:STAT|x,x|y
--   x is stat name
--   y is formula
data BonusStat = BonusStat { bonusStatNames :: [String]
                           , bonusStatFormula :: Formula }
               deriving (Show, Eq)

parseBonusStat :: PParser BonusStat
parseBonusStat = do
  _ <- bonusTag "STAT"
  bonusStatNames <- parseStringNoCommas `sepBy` char ','
  bonusStatFormula <- char '|' *> parseFormula
  return BonusStat { .. }

-- BONUS:STAT|%CHOICE
-- TODO: define.
parseBonusStatChoice :: PParser ()
parseBonusStatChoice = () <$ labeled "STAT|%CHOICE"

-- BONUS:SPELLCAST|x;y|z
data BonusSpellCastType = SpellCastClassName String
                        | SpellCastSpellType String
                          deriving (Show, Eq)

data BonusSpellCastSlot = SpellCastLevelNumber Int
                        | SpellCastLevelAll
                        | SpellCastLevelChoice
                          deriving (Show, Eq)

data BonusSpellCast = BonusSpellCast { bonusSpellCastType :: BonusSpellCastType
                                     , bonusSpellCastSlot :: BonusSpellCastSlot
                                     , bonusSpellCastNumber :: Int }
                        deriving (Show, Eq)

parseBonusSpellCast :: PParser BonusSpellCast
parseBonusSpellCast = do
  _ <- bonusTag "SPELLCAST"
  bonusSpellCastType <- parseBonusSpellCastType <* char ';'
  bonusSpellCastSlot <- parseBonusSpellCastSlot <* char '|'
  bonusSpellCastNumber <- textToInt <$> manyNumbers
  return BonusSpellCast { .. } where
    parseBonusSpellCastType = (labeled "CLASS=" *> (SpellCastClassName <$> parseString))
                          <|> (labeled "TYPE=" *> (SpellCastSpellType <$> parseString))
    parseBonusSpellCastSlot = (SpellCastLevelAll <$ labeled "LEVEL=ALL")
                          <|> (SpellCastLevelChoice <$ labeled "LEVEL=%CHOICE")
                          <|> (labeled "LEVEL=" *> (SpellCastLevelNumber <$> slotToInt))
    slotToInt = textToInt <$> manyNumbers

-- BONUS:SPELLCASTMULT|x;y|z
--   x is class name or spell type
--   y is level number
--   z is number of spells
data BonusSpellCastMultType = SpellCastMultClassName String
                            | SpellCastMultSpellType String
                              deriving (Show, Eq)

data BonusSpellCastMult = BonusSpellCastMult { bonusSpellCastMultType :: BonusSpellCastMultType
                                             , bonusSpellCastMultLevel :: Int
                                             , bonusSpellCastMultNumber :: Int }
                        deriving (Show, Eq)

parseBonusSpellCastMult :: PParser BonusSpellCastMult
parseBonusSpellCastMult = do
  _ <- bonusTag "SPELLCASTMULT"
  bonusSpellCastMultType <- parseBonusSpellCastMultType <* char ';'
  bonusSpellCastMultLevel <- textToInt <$> (labeled "LEVEL=" *> manyNumbers)
  _ <- char '|'
  bonusSpellCastMultNumber <- textToInt <$> manyNumbers
  return BonusSpellCastMult { .. } where
    parseBonusSpellCastMultType = (labeled "CLASS=" *> (SpellCastMultClassName <$> parseString))
                              <|> (labeled "TYPE=" *> (SpellCastMultSpellType <$> parseString))

-- BONUS:SPELLKNOWN|x;y|z
--   x is class name or spell type
--   y is level number
--   z is number of spells
parseBonusSpellKnown :: PParser BonusSpellCastMult
parseBonusSpellKnown = do
  _ <- bonusTag "SPELLKNOWN"
  bonusSpellCastMultType <- parseBonusSpellCastMultType <* char ';'
  bonusSpellCastMultLevel <- textToInt <$> (labeled "LEVEL=" *> manyNumbers)
  _ <- char '|'
  bonusSpellCastMultNumber <- textToInt <$> manyNumbers
  return BonusSpellCastMult { .. } where
    parseBonusSpellCastMultType = (labeled "CLASS=" *> (SpellCastMultClassName <$> parseString))
                              <|> (labeled "TYPE=" *> (SpellCastMultSpellType <$> parseString))

-- BONUS:VAR|x,x,...|y
--   x is variable name
--   y is number, variable, or formula to adjust variable by
data BonusVar = BonusVar { bonusVariables :: [String]
                         , adjustBy :: Formula }
              deriving (Show, Eq)

parseBonusVariable :: PParser BonusVar
parseBonusVariable = do
  _ <- bonusTag "VAR"
  bonusVariables <- parseString `sepBy` char ','
  adjustBy <- char '|' *> parseFormula
  return BonusVar { .. }

-- BONUS:VISION|x|y
--   x is vision type
--   y is formula
data BonusVisionData = BonusVisionData { bonusVision :: String
                                       , bonusVisionFormula :: Formula }
                 deriving (Show, Eq)

parseBonusVision :: PParser BonusVisionData
parseBonusVision = do
  _ <- bonusTag "VISION"
  bonusVision <- parseTill '|'
  bonusVisionFormula <- parseFormula
  return BonusVisionData { .. }

-- BONUS:WEAPON|x,x|y
--   x is weapon property
--   y is number, variable, or formula to add
data BonusWeaponProp = BonusWeaponProp { bonusWeaponProperties :: [BonusWeaponProperty]
                                       , bonusWeaponFormula :: Formula }
                     deriving (Show, Eq)

data BonusWeaponProperty = P_ATTACKS
                         | P_ATTACKSPROGRESS
                         | P_CRITRANGEADD -- undocumented
                         | P_CRITRANGEDOUBLE -- undocumented
                         | P_DAMAGE
                         | P_DAMAGEMULT Int
                         | P_DAMAGESIZE
                         | P_DAMAGESHORTRANGE
                         | P_TOHIT
                         | P_TOHITSHORTRANGE
                         | P_WEAPONBAB
                         | P_WIELDCATEGORY
                           deriving (Show, Eq)

parseBonusWeaponProp :: PParser BonusWeaponProp
parseBonusWeaponProp = do
  _ <- bonusTag "WEAPON"
  bonusWeaponProperties <- parseWeaponProperty `sepBy` char ','
  bonusWeaponFormula <- char '|' *> parseFormula
  return BonusWeaponProp { .. } where
    parseWeaponProperty = try (P_ATTACKSPROGRESS <$ labeled "ATTACKSPROGRESS")
                      <|> try (P_ATTACKS <$ labeled "ATTACKS")
                      <|> try (P_CRITRANGEADD <$ labeled "CRITRANGEADD")
                      <|> try (P_CRITRANGEDOUBLE <$ labeled "CRITRANGEDOUBLE")
                      <|> try (labeled "DAMAGEMULT:" *> (P_DAMAGEMULT <$> parseInt))
                      <|> try (P_DAMAGESIZE <$ labeled "DAMAGESIZE")
                      <|> try (P_DAMAGESHORTRANGE <$ labeled "DAMAGE-SHORTRANGE")
                      <|> try (P_DAMAGE <$ labeled "DAMAGE")
                      <|> try (P_TOHITSHORTRANGE <$ labeled "TOHIT-SHORTRANGE")
                      <|> try (P_TOHIT <$ labeled "TOHIT")
                      <|> try (P_WEAPONBAB <$ labeled "WEAPONBAB")
                      <|> (P_WIELDCATEGORY <$ labeled "WIELDCATEGORY")
    parseInt = textToInt <$> manyNumbers

-- BONUS:WEAPONPROF=x|y,y...|z
--   x is weapon proficiency name or type
--   y is weapon property
--   z is number, variable, or formula to add
data BonusWeapon = WeaponName String
                 | WeaponType String
                   deriving (Show, Eq)

data BonusWeaponProfProperty = CRITMULTADD
                             | CRITRANGEADD
                             | CRITRANGEDOUBLE
                             | DAMAGE
                             | DAMAGEMULT
                             | DAMAGESIZE
                             | DAMAGESHORTRANGE
                             | PCSIZE
                             | REACH
                             | TOHIT
                             | TOHITSHORTRANGE
                             | TOHITOVERSIZE
                             | WIELDCATEGORY
                               deriving (Show, Eq)

data BonusWeaponProf = BonusWeaponProf { bonusWeaponProficency :: BonusWeapon
                                       , bonusWeaponProfProperties :: [BonusWeaponProfProperty]
                                       , bonusWeaponProfFormulas :: Formula }
                     deriving (Show, Eq)

parseBonusWeaponProf :: PParser BonusWeaponProf
parseBonusWeaponProf = do
  _ <- labeled "WEAPONPROF="
  bonusWeaponProficency <- parseWeaponProficiency <* char '|'
  -- moderndispatch075_equip_armorshields.lst has no weapon property
  bonusWeaponProfProperties <- option [] ((parseWeaponProperty `sepBy` char ',') <* char '|')
  bonusWeaponProfFormulas <- parseFormula
  return BonusWeaponProf { .. } where
    parseWeaponProficiency = try (labeled "TYPE=" >> (WeaponType <$> parseString))
                         <|> try (labeled "TYPE." >> (WeaponType <$> parseString))
                         <|> WeaponName <$> parseString
    parseWeaponProperty = try (CRITMULTADD <$ labeled "CRITMULTADD")
                      <|> try (CRITRANGEADD <$ labeled "CRITRANGEADD")
                      <|> try (CRITRANGEDOUBLE <$ labeled "CRITRANGEDOUBLE")
                      <|> try (DAMAGEMULT <$ labeled "DAMAGEMULT")
                      <|> try (DAMAGESIZE <$ labeled "DAMAGESIZE")
                      <|> try (DAMAGESHORTRANGE <$ labeled "DAMAGESHORTRANGE")
                      <|> try (DAMAGE <$ labeled "DAMAGE")
                      <|> try (PCSIZE <$ labeled "PCSIZE")
                      <|> try (REACH <$ labeled "REACH")
                      <|> try (TOHITSHORTRANGE <$ labeled "TOHITSHORTRANGE")
                      <|> try (TOHITOVERSIZE <$ labeled "TOHITOVERSIZE")
                      <|> try (TOHIT <$ labeled "TOHIT")
                      <|> (WIELDCATEGORY <$ labeled "WIELDCATEGORY")

-- BONUS:UDAM|x|y
--   x is CLASS.text
--   y is formula
data UnarmedDamage = UnarmedDamage { className :: String
                                   , unarmedFormula :: Formula }
                     deriving (Show, Eq)

parseBonusUnarmedDamage :: PParser UnarmedDamage
parseBonusUnarmedDamage = do
  _ <- bonusTag "UDAM"
  className <- parseTill '|'
  unarmedFormula <- parseFormula
  return UnarmedDamage { .. }

-- TEMPBONUS:x,x,...|y|z
--   x is PC, ANYPC, or EQ
--   y is equipment type (only when x==EQ)
--   z is bonus subtoken
data Target = PC | ANYPC | EQUIPMENT
              deriving (Show, Eq)

data TempBonus = TempBonus { target :: Target
                           , equipmentType :: Maybe String
                           , additionalBonuses :: [Bonus]
                           -- TODO: can this ever apply to temp bonus itself?
                           , additionalRestrictions :: [RestrictionTag] }
                 deriving (Show, Eq)

parseTemporaryBonus :: PParser TempBonus
parseTemporaryBonus = do
  _ <- tag "TEMPBONUS"
  target <- parseTarget
  equipmentType <- tryOption $ parseEquipmentType target
  -- additionalBonuses <- char '|' >> parseAnyBonus `sepBy` char '|'
  additionalBonuses <- many $ try bonuses
  additionalRestrictions <- option [] parseAdditionalRestrictions
  return TempBonus { .. } where
    parseTarget :: PParser Target
    parseTarget = (PC <$ labeled "PC")
              <|> (ANYPC <$ labeled "ANYPC")
              <|> (EQUIPMENT <$ labeled "EQ")
    parseEquipmentType EQUIPMENT = parseString
    parseEquipmentType _ = string "\t" -- better way to do this?
    bonuses = char '|' >> parseAnyBonus

-- TEMPDESC:x
--   x is text to display in the temporary bonus sub-tab
parseBonusDescription :: PParser String
parseBonusDescription = tag "TEMPDESC" >> restOfTag

parseAnyBonus :: PParser Bonus
parseAnyBonus = BonusSkillRank <$> parseBonusSkillRank
            <|> BonusVariable <$> parseBonusVariable
            <|> BonusSkillChoice <$> parseBonusSkillChoice
            <|> BonusSkill <$> parseBonusSkill
            <|> BonusAbilityPool <$> parseBonusAbilityPool
            <|> BonusCasterLevel <$> parseBonusCasterLevel
            <|> BonusCheck <$> parseBonusCheck
            <|> BonusCombat <$> parseBonusCombat
            <|> BonusDamageReduction <$> parseBonusDR
            <|> BonusModifyEquipmentPenalty <$> parseBonusEquipmentPenalty
            <|> BonusModifyEquipmentRange <$> parseBonusEquipmentRange
            <|> BonusModifyEquipmentWeight <$> parseBonusEquipmentWeight
            <|> BonusHitPoint <$> parseBonusHP
            <|> BonusItemCost <$> parseBonusItemCost
            <|> BonusSizeModifier <$> parseBonusSizeMod
            <|> BonusAddSave <$> parseBonusSave
            <|> BonusAddSituation <$> parseBonusSituation
            <|> BonusDifficultyClass <$> parseBonusDC
            <|> BonusMiscellany <$> parseBonusMisc
            <|> BonusMovement <$> parseBonusMoveAdd
            <|> BonusMovementMultiplier <$> parseBonusMoveMultiplier
            <|> BonusVision <$> parseBonusVision
            <|> BonusSlotItems <$> parseBonusSlots
            <|> BonusSpellCasting <$> parseBonusSpellCast
            <|> BonusSpellCastingMultiple <$> parseBonusSpellCastMult
            <|> BonusSpellKnown <$> parseBonusSpellKnown
            <|> BonusSkillPoints <$> parseBonusSkillPoints
            <|> BonusPostMoveAddition <$> parseBonusPostMoveAdd
            <|> BonusCharacterStatChoice <$> parseBonusStatChoice
            <|> BonusCharacterStat <$> parseBonusStat
            <|> BonusWeaponProficency <$> parseBonusWeaponProf
            <|> BonusWeaponProperty <$> parseBonusWeaponProp
            <|> BonusUnarmedDamage <$> parseBonusUnarmedDamage

parseRawBonus :: PParser Bonus
parseRawBonus = (tag "BONUS" *> parseAnyBonus)
            <|> TemporaryBonus <$> parseTemporaryBonus
            <|> BonusDescription <$> parseBonusDescription

parseBonus :: PParser BonusTag
parseBonus = do
  bonus <- parseRawBonus
  (bonusRestrictions, bonusType) <- parseBonusRestrictionsAndType
  return BonusTag { .. }
