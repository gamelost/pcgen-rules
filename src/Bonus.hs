{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Control.Exception
import Data.Maybe
import JEPFormula
import Restrictions(Restriction, parseAdditionalRestrictions)
import Common

data Bonus = BonusSkill Skill
           | BonusSkillRank SkillRank
           | BonusVariable BonusVar
           | BonusWeaponProficency BonusWeaponProf
           | BonusAbilityPool BonusAbility
           | BonusDescription T.Text
           | TemporaryBonus TempBonus
             deriving (Show, Eq)

-- BONUS:ABILITYPOOL|x|y
--   x is ability category
--   y is formula added to pool (not number!)
data BonusAbility = BonusAbility { abilityCategory :: T.Text
                                 , abilityPoolFormula :: Formula }
                  deriving (Show, Eq)

parseBonusAbilityPool :: Parser BonusAbility
parseBonusAbilityPool = do
  _ <- string "ABILITYPOOL|"
  abilityCategory <- parseString
  abilityPoolFormula <- char '|' *> parseFormula
  return BonusAbility { .. }

-- BONUS:SKILL:x,x,...|y
--   x is LIST, ALL, skill name, stat name (STAT.x), skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkill = List
                  | All
                  | BonusSkillName T.Text
                  | BonusSkillType T.Text
                  | StatName T.Text
                    deriving (Show, Eq)

data SkillFormulaType = SkillFormula Formula
                      | SkillText T.Text
                        deriving (Show, Eq)

data Skill = Skill { bonusToSkills :: [BonusToSkill]
                   , skillFormula :: SkillFormulaType
                   , skillType :: Maybe (T.Text, Bool)
                   , skillRestrictions :: [Restriction] }
             deriving (Show, Eq)

-- we have far more bonus types, but for now, stick with a simple (Text, Bool)
parseBonusType :: Parser (T.Text, Bool)
parseBonusType = do
  bonusType <- (string "|TYPE=" <|> string "|SKILLTYPE=") *> parseString
  let testForStack = T.stripSuffix ".STACK" bonusType
  return (fromMaybe bonusType testForStack, isJust testForStack)

-- bonus types can be found either before or after restrictions
parseBonusRestrictionsAndType :: Parser ([Restriction], Maybe (T.Text, Bool))
parseBonusRestrictionsAndType = do
  type1 <- optional parseBonusType
  restrictions <- optional parseAdditionalRestrictions
  type2 <- optional parseBonusType
  -- make sure we didn't parse bonus TYPEs both times!
  let _ = assert (isJust type1 && isJust type2)
  let bonusType = type1 <|> type2
  let bonusRestrictions = fromMaybe [] restrictions
  return (bonusRestrictions, bonusType)

parseBonusSkill :: Parser Skill
parseBonusSkill = do
  _ <- string "SKILL|"
  bonusToSkills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  (skillRestrictions, skillType) <- parseBonusRestrictionsAndType
  return Skill { .. } where
    parseBonusSkills = parseList
                   <|> parseAll
                   <|> parseSkillType
                   <|> parseStatName
                   <|> parseSkillName
    parseList = string "LIST" >> return List
    parseAll = string "ALL" >> return All
    parseSkillType = string "TYPE=" >> (BonusSkillType <$> parseStringNoCommas)
    parseStatName = string "STAT." >> (StatName <$> parseStringNoCommas)
    parseSkillName = BonusSkillName <$> parseStringNoCommas
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> parseStringNoCommas
    parseStringNoCommas = takeWhile1 $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~"

-- BONUS:SKILLRANK|x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName T.Text
                      | SkillRankType T.Text
                        deriving (Show, Eq)

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: SkillFormulaType
                           , skillRankType :: Maybe (T.Text, Bool)
                           , skillRankRestrictions :: [Restriction] }
                 deriving (Show, Eq)

parseBonusSkillRank :: Parser SkillRank
parseBonusSkillRank = do
  _ <- string "SKILLRANK|"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseSkillFormulaType
  (skillRankRestrictions, skillRankType) <- parseBonusRestrictionsAndType
  return SkillRank { .. } where
    parseBonusSkillRanks = many space >> (parseSkillType <|> parseSkillName)
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseStringNoCommas)
    parseSkillName = SkillRankName <$> parseStringNoCommas
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> (string "SKILLRANK=" >> parseStringNoCommas)
    parseStringNoCommas = takeWhile1 $ inClass "-A-Za-z0-9_ &+./:?!%#'()[]~"

-- BONUS:VAR|x,x,...|y
--   x is variable name
--   y is number, variable, or formula to adjust variable by
data BonusVar = BonusVar { bonusVariables :: [T.Text]
                         , adjustBy :: Formula
                         , bonusVarType :: Maybe (T.Text, Bool)
                         , bonusVarRestrictions :: [Restriction] }
              deriving (Show, Eq)

parseBonusVariable :: Parser BonusVar
parseBonusVariable = do
  _ <- string "VAR|"
  bonusVariables <- parseString `sepBy` char ','
  adjustBy <- char '|' *> parseFormula
  (bonusVarRestrictions, bonusVarType) <- parseBonusRestrictionsAndType
  return BonusVar { .. }

-- BONUS:WEAPONPROF=x|y,y...|z
--   x is weapon proficiency name or type
--   y is weapon property
--   z is number, variable, or formula to add
data BonusWeapon = WeaponName T.Text
                 | WeaponType T.Text
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

parseBonusWeaponProf :: Parser BonusWeaponProf
parseBonusWeaponProf = do
  _ <- string "WEAPONPROF="
  bonusWeaponProficency <- parseWeaponProficiency
  bonusWeaponProperties <- char '|' *> parseWeaponProperty `sepBy` char ','
  bonusWeaponFormula <- char '|' *> parseFormula
  return BonusWeaponProf { .. } where
    parseWeaponProficiency = string "TYPE=" >> (WeaponType <$> parseString)
                         <|> WeaponName <$> parseString
    parseWeaponProperty = (string "CRITMULTADD" >> return CRITMULTADD)
                      <|> (string "CRITRANGEADD" >> return CRITRANGEADD)
                      <|> (string "CRITRANGEDOUBLE" >> return CRITRANGEDOUBLE)
                      <|> (string "DAMAGE" >> return DAMAGE)
                      <|> (string "DAMAGEMULT" >> return DAMAGEMULT)
                      <|> (string "DAMAGESIZE" >> return DAMAGESIZE)
                      <|> (string "DAMAGESHORTRANGE" >> return DAMAGESHORTRANGE)
                      <|> (string "PCSIZE" >> return PCSIZE)
                      <|> (string "REACH" >> return REACH)
                      <|> (string "TOHIT" >> return TOHIT)
                      <|> (string "TOHITSHORTRANGE" >> return TOHITSHORTRANGE)
                      <|> (string "TOHITOVERSIZE" >> return TOHITOVERSIZE)
                      <|> (string "WIELDCATEGORY" >> return WIELDCATEGORY)

-- TEMPBONUS:x,x,...|y|z
--   x is PC, ANYPC, or EQ
--   y is equipment type (only when x==EQ)
--   z is bonus subtoken
data Target = PC | ANYPC | EQUIPMENT
              deriving (Show, Eq)

data TempBonus = TempBonus { target :: Target
                           , equipmentType :: Maybe T.Text
                           , additionalBonuses :: [Bonus]
                           , additionalRestrictions :: [Restriction] }
                 deriving (Show, Eq)

parseTemporaryBonus :: Parser TempBonus
parseTemporaryBonus = do
  _ <- tag "TEMPBONUS"
  target <- parseTarget
  equipmentType <- optional $ parseEquipmentType target
  additionalBonuses <- char '|' >> parseAnyBonus `sepBy` char '|'
  additionalRestrictions <- option [] parseAdditionalRestrictions
  return TempBonus { .. } where
    parseTarget :: Parser Target
    parseTarget = (string "PC" >> return PC)
              <|> (string "ANYPC" >> return ANYPC)
              <|> (string "EQ" >> return EQUIPMENT)
    parseEquipmentType EQUIPMENT = parseString
    parseEquipmentType _ = string "\t" -- better way to do this?

-- TEMPDESC:x
--   x is text to display in the temporary bonus sub-tab
parseBonusDescription :: Parser T.Text
parseBonusDescription = tag "TEMPDESC" >> restOfTag

parseAnyBonus :: Parser Bonus
parseAnyBonus = BonusSkillRank <$> parseBonusSkillRank
            <|> BonusVariable <$> parseBonusVariable
            <|> BonusSkill <$> parseBonusSkill
            <|> BonusAbilityPool <$> parseBonusAbilityPool
            <|> BonusWeaponProficency <$> parseBonusWeaponProf

parseBonus :: Parser Bonus
parseBonus = (tag "BONUS" *> parseAnyBonus)
         <|> TemporaryBonus <$> parseTemporaryBonus
         <|> BonusDescription <$> parseBonusDescription
