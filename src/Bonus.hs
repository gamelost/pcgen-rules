{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import JEPFormula
import Restrictions(Restriction, parseAdditionalRestrictions)
import Common

data Bonus = BonusSkill Skill
           | BonusSkillStack (Skill, [Restriction])
           | BonusSkillRank SkillRank
           | BonusDescription T.Text
           | BonusRestrictions [Restriction]
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
                   , skillStack :: Maybe T.Text } deriving Show

parseBonusSkill :: Parser Skill
parseBonusSkill = do
  _ <- string "BONUS:SKILL|"
  skills <- parseBonusSkills `sepBy` char ','
  skillFormula <- char '|' *> parseSkillFormulaType
  skillType <- option Nothing $ Just <$> (string "|TYPE=" >> parseString)
  skillStack <- return Nothing
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
  _ <- string ".STACK"
  let newSkill = skill { skillStack=Just what }
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
  skillRankType <- option Nothing $ Just <$> (string "|TYPE=" >> parseString)
  return SkillRank { .. } where
    parseBonusSkillRanks = parseSkillType <|> parseSkillName
    parseSkillType = string "TYPE=" >> (SkillRankType <$> parseString)
    parseSkillName = SkillRankName <$> parseString

-- TEMPDESC:x
--   x is text to display in the temporary bonus sub-tab
parseBonusDescription :: Parser T.Text
parseBonusDescription = tag "TEMPDESC" >> restOfTag

parseBonus :: Parser Bonus
parseBonus = BonusSkillStack <$> parseBonusSkillWithStack
         <|> BonusSkillRank <$> parseBonusSkillRank
         <|> BonusSkill <$> parseBonusSkill
         <|> BonusDescription <$> parseBonusDescription
         <|> BonusRestrictions <$> parseAdditionalRestrictions
