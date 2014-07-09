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
                   , skillType :: Maybe T.Text
                   , skillStack :: Bool
                   , skillRestrictions :: [Restriction] }
             deriving (Show, Eq)

parseBonusSkill :: Parser Skill
parseBonusSkill = do
  _ <- string "SKILL|"
  bonusToSkills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  skillType <- optional $ string "|TYPE=" >> parseString
  restrictions <- optional parseAdditionalRestrictions
  let skillRestrictions = fromMaybe [] restrictions
  let skillStack = False
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

-- this one is kind of ugly, because you can have:
-- BONUS:SKILL|...|...<restrictions>|TYPE=foo.STACK
-- or
-- BONUS:SKILL|...|TYPE=foo.STACK|...<restrictions>
-- so we have to account for both cases. ugh.
parseBonusSkillWithStack :: Parser Skill
parseBonusSkillWithStack = do
  skill <- parseBonusSkill
  what <- string "|TYPE=" *> parseWord
  isStack <- optional $ string ".STACK"
  -- make sure we don't override a previously set type, this is most likely a bug
  let _ = assert (isNothing $ skillType skill)
  let newSkill = skill { skillType=Just what
                       , skillStack=isJust isStack }
  return newSkill

-- BONUS:SKILLRANK:x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName T.Text
                      | SkillRankType T.Text
                        deriving (Show, Eq)

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: Formula
                           , skillRankType :: Maybe T.Text
                           , skillRankRestrictions :: [Restriction] }
                 deriving (Show, Eq)

parseBonusSkillRank :: Parser SkillRank
parseBonusSkillRank = do
  _ <- string "SKILLRANK|"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseFormula
  skillRankType <- optional $ string "|TYPE=" >> parseString
  skillRankRestrictions <- parseAdditionalRestrictions
  return SkillRank { .. } where
    parseBonusSkillRanks = parseSkillType <|> parseSkillName
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseString)
    parseSkillName = SkillRankName <$> parseString

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
            <|> BonusSkill <$> parseBonusSkillWithStack
            <|> BonusSkill <$> parseBonusSkill

parseBonus :: Parser Bonus
parseBonus = (tag "BONUS" *> parseAnyBonus)
         <|> TemporaryBonus <$> parseTemporaryBonus
         <|> BonusDescription <$> parseBonusDescription
