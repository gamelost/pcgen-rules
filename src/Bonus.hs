{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Control.Applicative hiding (optional, many)
import Control.Exception hiding (try)
import Data.Maybe
import JEPFormula
import Restrictions(Restriction, parseAdditionalRestrictions)
import Common

data Bonus = BonusSkill Skill
           | BonusSkillRank SkillRank
           | BonusVariable BonusVar
           | BonusWeaponProficency BonusWeaponProf
           | BonusAbilityPool BonusAbility
           | BonusDescription String
           | TemporaryBonus TempBonus
             deriving (Show, Eq)

-- BONUS:ABILITYPOOL|x|y
--   x is ability category
--   y is formula added to pool (not number!)
data BonusAbility = BonusAbility { abilityCategory :: String
                                 , abilityPoolFormula :: Formula }
                  deriving (Show, Eq)


bonusTag :: String -> PParser String
bonusTag t = labeled $ t ++ "|"

parseBonusAbilityPool :: PParser BonusAbility
parseBonusAbilityPool = do
  _ <- bonusTag "ABILITYPOOL"
  abilityCategory <- parseString
  abilityPoolFormula <- char '|' *> parseFormula
  return BonusAbility { .. }

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
                   , skillFormula :: SkillFormulaType
                   , skillType :: Maybe (String, Bool)
                   , skillRestrictions :: [Restriction] }
             deriving (Show, Eq)

-- we have far more bonus types, but for now, stick with a simple (Text, Bool)
parseBonusType :: PParser (String, Bool)
parseBonusType = do
  bonusType <- types *> parseString
  let testForStack = stripSuffix ".STACK" bonusType
  return (fromMaybe bonusType testForStack, isJust testForStack) where
    types = labeled "|TYPE="
        <|> labeled "|SKILLTYPE="

-- bonus types can be found either before or after restrictions
parseBonusRestrictionsAndType :: PParser ([Restriction], Maybe (String, Bool))
parseBonusRestrictionsAndType = do
  type1 <- tryOption parseBonusType
  restrictions <- tryOption parseAdditionalRestrictions
  type2 <- tryOption parseBonusType
  -- make sure we didn't parse bonus TYPEs both times!
  let _ = assert (isJust type1 && isJust type2)
  let bonusType = type1 <|> type2
  let bonusRestrictions = fromMaybe [] restrictions
  return (bonusRestrictions, bonusType)

parseBonusSkill :: PParser Skill
parseBonusSkill = do
  _ <- bonusTag "SKILL"
  bonusToSkills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  (skillRestrictions, skillType) <- parseBonusRestrictionsAndType
  return Skill { .. } where
    parseBonusSkills = parseList
                   <|> parseAll
                   <|> parseSkillType
                   <|> parseStatName
                   <|> parseSkillName
    parseList = labeled "LIST" >> return List
    parseAll = labeled "ALL" >> return All
    parseSkillType = labeled "TYPE=" >> (BonusSkillType <$> parseStringNoCommas)
    parseStatName = labeled "STAT." >> (StatName <$> parseStringNoCommas)
    parseSkillName = BonusSkillName <$> parseStringNoCommas
    parseSkillFormulaType = SkillFormula <$> try parseFormula
                        <|> SkillText <$> parseStringNoCommas
    parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~"

-- BONUS:SKILLRANK|x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName String
                      | SkillRankType String
                        deriving (Show, Eq)

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: SkillFormulaType
                           , skillRankType :: Maybe (String, Bool)
                           , skillRankRestrictions :: [Restriction] }
                 deriving (Show, Eq)

parseBonusSkillRank :: PParser SkillRank
parseBonusSkillRank = do
  _ <- bonusTag "SKILLRANK"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseSkillFormulaType
  (skillRankRestrictions, skillRankType) <- parseBonusRestrictionsAndType
  return SkillRank { .. } where
    parseBonusSkillRanks = many space >> (parseSkillType <|> parseSkillName)
    parseSkillType = labeled "TYPE=" >> (SkillRankType <$> parseStringNoCommas)
    parseSkillName = SkillRankName <$> parseStringNoCommas
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> (labeled "SKILLRANK=" >> parseStringNoCommas)
    parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~"

-- BONUS:VAR|x,x,...|y
--   x is variable name
--   y is number, variable, or formula to adjust variable by
data BonusVar = BonusVar { bonusVariables :: [String]
                         , adjustBy :: Formula
                         , bonusVarType :: Maybe (String, Bool)
                         , bonusVarRestrictions :: [Restriction] }
              deriving (Show, Eq)

parseBonusVariable :: PParser BonusVar
parseBonusVariable = do
  _ <- bonusTag "VAR"
  bonusVariables <- parseString `sepBy` char ','
  adjustBy <- char '|' *> parseFormula
  (bonusVarRestrictions, bonusVarType) <- parseBonusRestrictionsAndType
  return BonusVar { .. }

-- BONUS:WEAPONPROF=x|y,y...|z
--   x is weapon proficiency name or type
--   y is weapon property
--   z is number, variable, or formula to add
data BonusWeapon = WeaponName String
                 | WeaponType String
                   deriving (Show, Eq)

data BonusWeaponProperty = CRITMULTADD
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
                                       , bonusWeaponProperties :: [BonusWeaponProperty]
                                       , bonusWeaponFormula :: Formula }
                     deriving (Show, Eq)

parseBonusWeaponProf :: PParser BonusWeaponProf
parseBonusWeaponProf = do
  _ <- labeled "WEAPONPROF="
  bonusWeaponProficency <- parseWeaponProficiency
  bonusWeaponProperties <- char '|' *> parseWeaponProperty `sepBy` char ','
  bonusWeaponFormula <- char '|' *> parseFormula
  return BonusWeaponProf { .. } where
    parseWeaponProficiency = labeled "TYPE=" >> (WeaponType <$> parseString)
                         <|> WeaponName <$> parseString
    parseWeaponProperty = (labeled "CRITMULTADD" >> return CRITMULTADD)
                      <|> (labeled "CRITRANGEADD" >> return CRITRANGEADD)
                      <|> (labeled "CRITRANGEDOUBLE" >> return CRITRANGEDOUBLE)
                      <|> (labeled "DAMAGE" >> return DAMAGE)
                      <|> (labeled "DAMAGEMULT" >> return DAMAGEMULT)
                      <|> (labeled "DAMAGESIZE" >> return DAMAGESIZE)
                      <|> (labeled "DAMAGESHORTRANGE" >> return DAMAGESHORTRANGE)
                      <|> (labeled "PCSIZE" >> return PCSIZE)
                      <|> (labeled "REACH" >> return REACH)
                      <|> (labeled "TOHIT" >> return TOHIT)
                      <|> (labeled "TOHITSHORTRANGE" >> return TOHITSHORTRANGE)
                      <|> (labeled "TOHITOVERSIZE" >> return TOHITOVERSIZE)
                      <|> (labeled "WIELDCATEGORY" >> return WIELDCATEGORY)

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
                           , additionalRestrictions :: [Restriction] }
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
    parseTarget = (labeled "PC" >> return PC)
              <|> (labeled "ANYPC" >> return ANYPC)
              <|> (labeled "EQ" >> return EQUIPMENT)
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
            <|> BonusSkill <$> parseBonusSkill
            <|> BonusAbilityPool <$> parseBonusAbilityPool
            <|> BonusWeaponProficency <$> parseBonusWeaponProf

parseBonus :: PParser Bonus
parseBonus = (tag "BONUS" *> parseAnyBonus)
         <|> TemporaryBonus <$> parseTemporaryBonus
         <|> BonusDescription <$> parseBonusDescription
