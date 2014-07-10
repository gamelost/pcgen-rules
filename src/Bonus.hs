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
           | BonusDescription T.Text
           | TemporaryBonus TempBonus
             deriving (Show, Eq)

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
  bonusType <- string "|TYPE=" *> parseString
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
    parseSkillType = string "TYPE=" >> (BonusSkillType <$> parseString)
    parseStatName = string "STAT." >> (StatName <$> parseString)
    parseSkillName = BonusSkillName <$> parseString
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> parseString

-- BONUS:SKILLRANK:x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName T.Text
                      | SkillRankType T.Text
                        deriving (Show, Eq)

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: Formula
                           , skillRankType :: Maybe (T.Text, Bool)
                           , skillRankRestrictions :: [Restriction] }
                 deriving (Show, Eq)

parseBonusSkillRank :: Parser SkillRank
parseBonusSkillRank = do
  _ <- string "SKILLRANK|"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseFormula
  (skillRankRestrictions, skillRankType) <- parseBonusRestrictionsAndType
  return SkillRank { .. } where
    parseBonusSkillRanks = parseSkillType <|> parseSkillName
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseString)
    parseSkillName = SkillRankName <$> parseString

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

parseBonus :: Parser Bonus
parseBonus = (tag "BONUS" *> parseAnyBonus)
         <|> TemporaryBonus <$> parseTemporaryBonus
         <|> BonusDescription <$> parseBonusDescription
