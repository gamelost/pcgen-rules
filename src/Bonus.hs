{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import JEPFormula
import Restrictions(Restriction, parseAdditionalRestrictions)
import Common

data BonusToSkill = List
                  | All
                  | SkillName T.Text
                  | SkillType T.Text
                  | StatName T.Text
                    deriving Show

data BonusToSkillRank = SkillRankName T.Text
                      | SkillRankType T.Text
                        deriving Show

data Skill = Skill { skills :: [BonusToSkill]
                   , skillFormula :: Formula } deriving Show

data SkillRank = SkillRank { skillRanks :: [BonusToSkillRank]
                           , skillRankFormula :: Formula } deriving Show

data Bonus = BonusSkill Skill
           | BonusSkillStack (Skill, T.Text, [Restriction])
           | BonusSkillRank SkillRank
           | BonusRestrictions [Restriction]
             deriving Show

-- BONUS:SKILL:x,x,...|y
--   x is LIST, ALL, skill name, stat name (STAT.x), skill type (TYPE=x)
--   y is number, variable, formula
parseBonusSkill :: Parser Skill
parseBonusSkill = do
  _ <- string "BONUS:SKILL|"
  skills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseFormula
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

-- this one is kind of ugly, because you can have:
-- BONUS:SKILL|...|...<restrictions>|TYPE=foo.STACK
-- or
-- BONUS:SKILL|...|TYPE=foo.STACK|...<restrictions>
-- so we have to account for both cases. ugh.
parseBonusSkillWithStack :: Parser (Skill, T.Text, [Restriction])
parseBonusSkillWithStack = do
  skill <- parseBonusSkill
  restrictions <- option [] parseAdditionalRestrictions
  what <- string "|TYPE=" *> parseWord
  _ <- string ".STACK"
  return (skill, what, restrictions)

-- BONUS:SKILLRANK:x,x,...|y
--   x is skill name, skill type (TYPE=x)
--   y is number, variable, formula
parseBonusSkillRank :: Parser SkillRank
parseBonusSkillRank = do
  _ <- string "BONUS:SKILLRANK|"
  skillRanks <- parseBonusSkillRanks `sepBy` char ','
  skillRankFormula <- char '|' *> parseFormula
  return SkillRank { .. } where
    parseBonusSkillRanks = parseSkillType <|> parseSkillName
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseString)
    parseSkillName = SkillRankName <$> parseString

parseBonus :: Parser Bonus
parseBonus = BonusSkillStack <$> parseBonusSkillWithStack
         <|> BonusSkillRank <$> parseBonusSkillRank
         <|> BonusSkill <$> parseBonusSkill
         <|> BonusRestrictions <$> parseAdditionalRestrictions
