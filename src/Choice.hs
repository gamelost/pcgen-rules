{-# LANGUAGE RecordWildCards #-}

module Choice where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, many1, notFollowedBy, option)
import Text.Parsec.Prim (try)
import ClassyPrelude hiding (try)

import JEPFormula
import Common

-- most of the parsers are stubbed out -- not sure how to deal with
-- choices in general atm.

data ChoiceTag = ChooseLanguageTag [ChooseLanguage]
               | ChooseNumberChoicesTag Choices
               | ChooseNumberTag ChooseNumber
               | ChooseManyNumbersTag ChooseManyNumbers
               | ChooseNoChoice ()
               | ChooseSkillTag [ChooseSkill]
               | ChooseEqBuilder EqBuilder
               | ChooseSchools [ChooseSchoolType]
               | ChooseString StringBuilder
               | ChoosePCStat [PCStat]
               | ChooseUserInput UserInput
               -- stubbed out
               | ChooseSpells ()
               | ChooseSpellLevel ()
               | ChooseAbility ()
               | ChooseClass ()
               | ChooseEquipment ()
               | ChooseFeat ()
               | ChooseFeatSelection ()
               | ChooseStatBonus ()
               | ChooseSkillBonus ()
               | ChooseRace ()
               | ChooseArmorProfBonus ()
               | ChooseWeaponProfBonus ()
               | ChooseShieldProfBonus ()
                 deriving (Show, Eq)

-- not fully implemented
data ChooseLanguage = ChoiceLanguage String
                    | ChoiceLanguageType String
                      deriving (Show, Eq)

parseChooseLanguage :: PParser [ChooseLanguage]
parseChooseLanguage = do
  _ <- labeled "CHOOSE:LANG|"
  parseChoiceLang `sepBy` char ',' where
    parseChoiceLang = ChoiceLanguageType <$> (labeled "TYPE=" *> parseString)
                  <|> ChoiceLanguage <$> parseString

parseChooseNoChoice :: PParser ()
parseChooseNoChoice = () <$ labeled "CHOOSE:NOCHOICE"

-- not fully implemented
data ChoiceType = AllChoices
                | ChoiceType String
                | ChoiceTitle String -- this is probably incorrect
                | ChoiceName String
                  deriving (Show, Eq)

data Choices = Choices { choiceNumber :: Int
                       , choiceSelection :: String
                       , choices :: [ChoiceType] }
                   deriving (Show, Eq)

parseChooseNumChoices :: PParser Choices
parseChooseNumChoices = do
  _ <- labeled "CHOOSE:NUMCHOICES="
  choiceNumber <- parseInteger <* char '|'
  choiceSelection <- parseString <* char '|'
  choices <- parseChoicesIfSelection choiceSelection
  return Choices { .. } where
    parseChoicesIfSelection "NOCHOICE" = return []
    parseChoicesIfSelection _ = parseChoiceString `sepBy` char '|'
    parseChoiceString = AllChoices <$ labeled "ALL"
                    <|> (labeled "TYPE=" *> (ChoiceType <$> parseString))
                    <|> (labeled "TITLE=" *> (ChoiceTitle <$> parseString))
                    <|> (ChoiceName <$> parseString)

-- not fully implemented
-- CHOOSE:NUMBER|v|w|x|y|z
--   not implemented. (NOSIGN/MULTIPLE seems to be undocumented)
data ChooseNumber = ChooseNumber { chooseMin :: Formula
                                 , chooseMax :: Formula
                                 , chooseTitle :: String }
                    deriving (Show, Eq)

parseChooseNumber :: PParser ChooseNumber
parseChooseNumber = do
  _ <- labeled "CHOOSE:NUMBER"
  chooseMin <- labeled "|MIN=" *> parseFormula
  chooseMax <- labeled "|MAX=" *> parseFormula
  chooseTitle <- labeled "|TITLE=" *> parseStringSemicolon
  return ChooseNumber { .. } where
   parseStringSemicolon = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~;"

data ChooseManyNumbers = ChooseManyNumbers { chooseManyNumbers :: [Int]
                                           , chooseManyMultiple :: Bool
                                           , chooseManyTitle :: String }
                       deriving (Show, Eq)

parseChooseManyNumbers :: PParser ChooseManyNumbers
parseChooseManyNumbers = do
  _ <- labeled "CHOOSE:NUMBER"
  chooseManyNumbers <- parseInteger `sepBy` char '|'
  chooseManyMultiple <- option False (True <$ labeled "MULTIPLE|")
  chooseManyTitle <- labeled "|TITLE=" *> parseString
  return ChooseManyNumbers { .. }

-- not fully implemented
data ChooseSkill = ChoiceSkill String
                 | ChoiceSkillType String
                 | ChoiceSkillTitle String
                 | ChoiceSkillFeat String
                 | ChoiceSkillRanks Int
                   deriving (Show, Eq)

parseChooseSkill :: PParser [ChooseSkill]
parseChooseSkill = do
  _ <- labeled "CHOOSE:SKILL|"
  parseChoiceSkill `sepBy` char '|' where
    parseChoiceSkill = ChoiceSkillType <$> (labeled "TYPE=" *> parseString)
                   <|> ChoiceSkillTitle <$> (labeled "TITLE=" *> parseString)
                   <|> ChoiceSkillFeat <$> (labeled "FEAT=" *> parseString)
                   <|> ChoiceSkillRanks <$> (labeled "RANKS=" *> parseInteger)
                   <|> ChoiceSkill <$> parseString

-- CHOOSE:EQBUILDER.SPELL|w|x|y|z
--   w is optional text
--   x is optional spell type (not used)
--   y is optional minimum level
--   z is optional maximum level
data EqBuilder = EqBuilder { eqBuilderText :: Maybe String
                           , eqBuilderMinimumLevel :: Formula
                           , eqBuilderMaximumLevel :: Formula }
               deriving (Show, Eq)

parseChooseEqBuilder :: PParser EqBuilder
parseChooseEqBuilder = do
  _ <- labeled "CHOOSE:EQBUILDER.SPELL"
  eqBuilderText <- tryOption $ char '|' *> parseString <* char '|'
  eqBuilderMinimumLevel <- option (Number 0) $ parseFormula <* char '|'
  eqBuilderMaximumLevel <- option (Variable "MAX_LEVEL") parseFormula
  return EqBuilder { .. }

-- CHOOSE:SCHOOLS|x|x|..
--   x is school name or feat or ALL
data ChooseSchoolType = SchoolName String
                      | FeatName String
                      | AllSchools
                        deriving (Show, Eq)

parseChooseSchools :: PParser [ChooseSchoolType]
parseChooseSchools = labeled "CHOOSE:SCHOOLS|" *> parseSchoolTypes `sepBy` char '|' where
  parseSchoolTypes = (labeled "FEAT=" *> (FeatName <$> parseString))
                 <|> (AllSchools <$ labeled "ALL")
                 <|> (SchoolName <$> parseString)

-- CHOOSE:PCSTAT|x|x|...
-- x is stat abbrevation, stat type or ALL
data PCStat = StatAbbrevation String
            | StatType String
            | NotStatType String
            | AllStats
              deriving (Show, Eq)

parseChoosePCStat :: PParser [PCStat]
parseChoosePCStat = labeled "CHOOSE:PCSTAT|" *> parsePCStat `sepBy` char '|' where
  parsePCStat = (labeled "TYPE=" *> (StatType <$> parseString))
            <|> (labeled "!TYPE=" *> (NotStatType <$> parseString))
            <|> (AllStats <$ labeled "ALL")
            <|> (StatAbbrevation <$> parseString)

-- CHOOSE:STRING|x|x..|y
--   x is choice to be offered
--   y is TITLE=text
data StringBuilder = StringBuilder { stringBuilderChoices :: [String]
                                   , stringBuilderTitle :: String }
                   deriving (Show, Eq)

parseChooseString :: PParser StringBuilder
parseChooseString = do
  _ <- labeled "CHOOSE:STRING"
  stringBuilderChoices <- many1 $ try $ char '|' *> parseChoiceString
  stringBuilderTitle <- option "Please pick a choice" $ labeled "|TITLE=" *> parseString
  return StringBuilder { .. } where
    parseChoiceString = notFollowedBy (labeled "TITLE=") *> parseString

-- CHOOSE:USERINPUT|x|y
--   x is number of inputs
--   y is chooser dialog title
data UserInput = UserInput { numberOfInputs :: Int
                           , userInputTitle :: String }
                   deriving (Show, Eq)

parseChooseUserInput :: PParser UserInput
parseChooseUserInput = do
  _ <- labeled "CHOOSE:USERINPUT|"
  numberOfInputs <- parseInteger
  userInputTitle <- labeled "TITLE=" *> parseString
  return UserInput { .. }

-- CHOOSE:ABILITY|w|x|y|y[x]|y[x,x]|x,y,y[x],y[x,x]
--   not implemented.
parseChooseAbility :: PParser ()
parseChooseAbility = () <$ (labeled "CHOOSE:ABILITY|" >> restOfTag)

-- CHOOSE:CLASS|x|y|y[x]|y[x,x]|x,y,y[x],y[x,x]
--   not implemented.
parseChooseClass :: PParser ()
parseChooseClass = () <$ (labeled "CHOOSE:CLASS|" >> restOfTag)

-- CHOOSE:EQUIPMENT
--   not implemented.
parseChooseEquipment :: PParser ()
parseChooseEquipment = () <$ (labeled "CHOOSE:EQUIPMENT|" >> restOfTag)

-- CHOOSE:FEAT|w|x|y|y[x]|y[x,x]|x,y,y[x],y[x,x]
--   not implemented.
parseChooseFeat :: PParser ()
parseChooseFeat = () <$ (labeled "CHOOSE:FEAT|" >> restOfTag)

-- CHOOSE:FEATSELECTION|w|x|y|y[z]|y[x,x]|x,y,y[x],y[x,x]
--   not implemented.
parseChooseFeatSelection :: PParser ()
parseChooseFeatSelection = () <$ (labeled "CHOOSE:FEATSELECTION|" >> restOfTag)

-- CHOOSE:SPELLS|x,y,y[z]|x|y|y[z;z]
--   not implemented.
parseChooseSpells :: PParser ()
parseChooseSpells = () <$ (labeled "CHOOSE:SPELLS|" >> restOfTag)

-- CHOOSE:SPELLLEVEL|w|x|y|z|x|y|z
--   not implemented.
parseChooseSpellLevel :: PParser ()
parseChooseSpellLevel = () <$ (labeled "CHOOSE:SPELLLEVEL|" >> restOfTag)

-- CHOOSE:STATBONUS|w|x|y|z
--   not implemented.
parseChooseStatBonus :: PParser ()
parseChooseStatBonus = () <$ (labeled "CHOOSE:STATBONUS|" >> restOfTag)

-- CHOOSE:SKILLBONUS|w|x|y|z
--   not implemented.
parseChooseSkillBonus :: PParser ()
parseChooseSkillBonus = () <$ (labeled "CHOOSE:SKILLBONUS|" >> restOfTag)

-- CHOOSE:RACE|x|y|y[x]|y[x,x]|x,y,y[x],y[x,x]
--   not implemented.
parseChooseRace :: PParser ()
parseChooseRace = () <$ (labeled "CHOOSE:RACE|" >> restOfTag)

-- CHOOSE:ARMORPROFICIENCY|x
--   not implemented (or documented).
parseChooseArmorProfBonus :: PParser ()
parseChooseArmorProfBonus = () <$ (labeled "CHOOSE:ARMORPROFICIENCY|" >> restOfTag)

-- CHOOSE:WEAPONPROFICIENCY|x
--   not implemented (or documented).
parseChooseWeaponProfBonus :: PParser ()
parseChooseWeaponProfBonus = () <$ (labeled "CHOOSE:WEAPONPROFICIENCY|" >> restOfTag)

-- CHOOSE:SHIELDPROFICIENCY|x
--   not implemented (or documented).
parseChooseShieldProfBonus :: PParser ()
parseChooseShieldProfBonus = () <$ (labeled "CHOOSE:SHIELDPROFICIENCY|" >> restOfTag)

parseChoice :: PParser ChoiceTag
parseChoice = ChooseLanguageTag <$> parseChooseLanguage
          <|> ChooseNumberChoicesTag <$> parseChooseNumChoices
          -- if this CHOOSE:NUMBER fails, try the next one
          <|> try (ChooseNumberTag <$> parseChooseNumber)
          <|> try (ChooseManyNumbersTag <$> parseChooseManyNumbers)
          <|> ChooseSkillTag <$> parseChooseSkill
          <|> ChooseNoChoice <$> parseChooseNoChoice
          <|> ChooseEqBuilder <$> parseChooseEqBuilder
          <|> ChooseString <$> parseChooseString
          <|> ChoosePCStat <$> parseChoosePCStat
          <|> ChooseUserInput <$> parseChooseUserInput
          <|> ChooseSchools <$> parseChooseSchools
          <|> ChooseSpells <$> parseChooseSpells
          <|> ChooseSpellLevel <$> parseChooseSpellLevel
          <|> ChooseAbility <$> parseChooseAbility
          <|> ChooseClass <$> parseChooseClass
          <|> ChooseEquipment <$> parseChooseEquipment
          <|> ChooseFeat <$> parseChooseFeat
          <|> ChooseFeatSelection <$> parseChooseFeatSelection
          <|> ChooseStatBonus <$> parseChooseStatBonus
          <|> ChooseSkillBonus <$> parseChooseSkillBonus
          <|> ChooseRace <$> parseChooseRace
          <|> ChooseArmorProfBonus <$> parseChooseArmorProfBonus
          <|> ChooseWeaponProfBonus <$> parseChooseWeaponProfBonus
          <|> ChooseShieldProfBonus <$> parseChooseShieldProfBonus
