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
           | BonusSkillStack (Skill, [Restriction])
           | BonusSkillRank SkillRank
           | BonusDescription T.Text
           | BonusRestrictions [Restriction]
           | TemporaryBonus TempBonus
             deriving Show

-- BONUS:SKILL:x,x,...|y
--   x is LIST, ALL, skill name, stat name (STAT.x), skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkill = List
                  | All
                  | SkillName T.Text
                  | SkillType T.Text
                  | StatName T.Text
                    deriving Show

data SkillFormulaType = SkillFormula Formula
                      | SkillText T.Text
                        deriving Show

data Skill = Skill { skills :: [BonusToSkill]
                   , skillFormula :: SkillFormulaType
                   , skillType :: Maybe T.Text
                   , skillStack :: Bool } deriving Show

parseBonusSkill :: Parser Skill
parseBonusSkill = do
  _ <- string "BONUS:SKILL|"
  skills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  skillType <- optionMaybe (string "|TYPE=" >> parseString)
  let skillStack = False
  return Skill { .. } where
    parseBonusSkills = parseList
                   <|> parseAll
                   <|> parseSkillType
                   <|> parseStatName
                   <|> parseSkillName
    parseList = string "LIST" >> return List
    parseAll = string "ALL" >> return All
    parseSkillType = string "TYPE=" >> (SkillType <$> parseString)
    parseStatName = string "STAT." >> (StatName <$> parseString)
    parseSkillName = SkillName <$> parseString
    parseSkillFormulaType = SkillFormula <$> parseFormula
                        <|> SkillText <$> parseString

-- this one is kind of ugly, because you can have:
-- BONUS:SKILL|...|...<restrictions>|TYPE=foo.STACK
-- or
-- BONUS:SKILL|...|TYPE=foo.STACK|...<restrictions>
-- so we have to account for both cases. ugh.
parseBonusSkillWithStack :: Parser (Skill, [Restriction])
parseBonusSkillWithStack = do
  skill <- parseBonusSkill
  restrictions <- option [] parseAdditionalRestrictions
  what <- string "|TYPE=" *> parseWord
  isStack <- optional $ string ".STACK"
  -- make sure we don't override a previously set type, this is most likely a bug
  let _ = assert (isNothing $ skillType skill)
  let newSkill = skill { skillType=Just what
                       , skillStack=isJust isStack }
  return (newSkill, restrictions)

-- BONUS:SKILLRANK:x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
data BonusToSkillRank = SkillRankName T.Text
                      | SkillRankType T.Text
                        deriving Show

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: Formula
                           , skillRankType :: Maybe T.Text } deriving Show

parseBonusSkillRank :: Parser SkillRank
parseBonusSkillRank = do
  _ <- string "BONUS:SKILLRANK|"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseFormula
  skillRankType <- optionMaybe (string "|TYPE=" >> parseString)
  return SkillRank { .. } where
    parseBonusSkillRanks = parseSkillType <|> parseSkillName
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseString)
    parseSkillName = SkillRankName <$> parseString

-- TEMPBONUS:x,x,...|y|z
--   x is PC, ANYPC, or EQ
--   y is equipment type (only when x==EQ)
--   z is bonus subtoken
data Target = PC | ANYPC | EQUIPMENT deriving Show

data TempBonus = TempBonus { target :: Target
                           , equipmentType :: Maybe T.Text
                           , additionalBonuses :: [Bonus]
                           , additionalRestrictions :: [Restriction] } deriving Show

-- does not work. examples:
-- TEMPBONUS:PC|SKILL|Craft (Fletcher)|-2|TYPE=Circumstance|!PREITEM:1,TYPE=WeaponsmithingTools
-- TEMPBONUS:PC|SKILL|Disguise|SynergyBonus|PRESKILL:1,Bluff=5|TYPE=TempSynergy'

parseTemporaryBonus :: Parser TempBonus
parseTemporaryBonus = do
  _ <- tag "TEMPBONUS"
  target <- parseTarget
  equipmentType <- optionMaybe $ parseEquipmentType target
  additionalBonuses <- char '|' >> parseBonus `sepBy` char '|'
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

parseBonus :: Parser Bonus
parseBonus = BonusSkillStack <$> parseBonusSkillWithStack
         <|> BonusSkillRank <$> parseBonusSkillRank
         <|> BonusSkill <$> parseBonusSkill
         <|> BonusDescription <$> parseBonusDescription
         <|> TemporaryBonus <$> parseTemporaryBonus
         <|> BonusRestrictions <$> parseAdditionalRestrictions
