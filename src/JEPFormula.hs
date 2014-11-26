{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , SkillType(..)
                  , Roll(..)
                  , parseFormula
                  , parseQuotedString
                  , parseInteger
                  , parseRolls
                  , evalJEPFormula
                  ) where

import qualified Data.Text as T
import qualified Data.Map as M

import Text.Parsec.Char (char, space, anyChar, satisfy)
import Text.Parsec.Combinator (manyTill, choice, sepBy, optional, option)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parsec.Prim (many, try)
import Control.Monad.State (State, get, msum)
import ClassyPrelude hiding (try, minimum, maximum, head)
import Prelude(minimum, maximum, head)

import Common

data Die = Die { number :: Int
               , sides :: Int
               , modifier :: Int }
               deriving (Show, Eq)

data Roll = RollDie Die
          | Special
            deriving (Show, Eq)

data Operand = Divide
             | Multiply
             | Subtract
             | Add
             | GreaterEqualsThan
               deriving (Show, Eq)

data SkillType = RANK
               | TOTALRANK
               | MODIFIER
               | STAT
               | MISC
               | TOTAL
                 deriving (Show, Eq)

data Formula = Number Int
             | Floating Float
             | Variable String
             | LookupSkill (SkillType, String)
             | Function String [Formula]
             | Group Formula
             | Negate Formula
             | Arithmetic Operand Formula Formula
               deriving (Show, Eq)

-- since we don't fully parse all variables yet, hard-code some to get
-- past the parser verification process
varBuiltins :: [String]
varBuiltins = [ "SynergyBonus"
              , "Reputation"
              , "DomainLVL"
              , "DomainPowerTimes"
              , "DomainAbilityTriggerLVL"
              , "Insanity"
              , "AlignmentAuraBase"
              , "ATWILL"
              , "AllowHolyAvenger"
              , "FlurryOfFistsExtraAttacks"
              , "FlurryAttacks"
              , "FlurryPenalty"
              , "FlurryBABBonus"
              , "FlurryExtraAttacks"
              , "MeditantFlurryBABBonus"
              , "MeditantFlurryExtraAttacks"
              , "MindBladeRANGE"
              , "MindBladeDAMAGESIZEADJ"
              , "MindBladeEnchantment"
              , "COUNT[EQTYPE.ARMOR.EQUIPPED]" -- ??
              , "SHIELDACCHECK"
              , "ACCHECK"
              , "EQHANDS"
              , "SKILLRANK=Bluff"
              , "AntipaladinLVL"
              , "PaladinLvl"
              , "PsiBladeDamage"
              , "PsiBladeEnhancement"
              , "PsiShieldDefense"
              , "STRSCORE"
              , "ShieldBonus"
              , "SamuraiArmorBonus"
              , "GunneryPenalty"
              , "RobotSpeedIncreaseCost"
              , "RobotPurchaseDC"
              , "InventionLevel"
              , "BaseInventionLevel"
              , "TrapCR"
              , "PSIONLEVEL"
              , "ShimmerMailACBonus"
              , "GunGlyphMarksman"
              , "MAXVEHICLEMODS"
              , "DMGDIE"
              , "%CHOICE" -- TODO: probably not right
              , "m_hp" -- ??
              , "unit" -- ??
              , "InventionLevel"
              , "SIZE"
              , "HEADPLUSTOTAL"
              , "PLUSTOTAL"
              , "VEHICLEWOUNDPOINTS"
              , "AlchemistBombAdditionalDice"
              , "DancingRobesArmorBonus"
              , "ArmorDPValue"
              , "DissonanceEnhancementBonusMain"
              , "DissonanceEnhancementBonusAlt"
              , "SeverisEnhancementBonus"
              , "BASECOST"
              , "WT"
              , "SPELLLEVEL"
              , "TL"
              , "CL"
              , "INT"
              , "DEX"
              , "STR"
              , "CON"
              , "WIS"
              , "CHA"
              , "NOB"
              ]

listOfFunctions :: [String]
listOfFunctions = [ "floor"
                  , "max"
                  , "min"
                  , "MIN"
                  , "MAX"
                  , "ceil"
                  ]

getVariables :: PParser [String]
getVariables = do
  vars <- get
  return $ M.keys vars ++ varBuiltins

variableParsers :: PParser String
variableParsers = msum . tryStrings =<< getVariables

functionParsers :: [PParser String]
functionParsers = tryStrings listOfFunctions

-- NB: PCGen rounds down to the nearest integer the results of each
-- formula. The problem is, of course, determining *when* exactly it
-- does this...
evalJEPFormula :: Variables -> Formula -> Int
evalJEPFormula vars f = floor $ evalJEPFormulae vars f

builtInFunction :: String -> [Rational] -> Rational
builtInFunction "min" = minimum
builtInFunction "max" = maximum
builtInFunction "MIN" = minimum
builtInFunction "MAX" = maximum
builtInFunction "floor" = \x ->
  case length x of
    1 -> (toRational :: Int -> Rational) . floor $ head x
    _ -> error "floor was called with incorrect arity"
builtInFunction "ceil" = \x ->
  case length x of
    1 -> (toRational :: Int -> Rational) . ceiling $ head x
    _ -> error "ceil was called with incorrect arity"
builtInFunction _ = error "No such built-in function"

evalJEPFormulae :: Variables -> Formula -> Rational
evalJEPFormulae _ (Number x) = toRational x
evalJEPFormulae _ (Floating x) = toRational x
evalJEPFormulae _ (LookupSkill _) =
  warning "evaluating skillinfo() is not implemented"
  0
evalJEPFormulae vars (Negate f) =
  negate $ evalJEPFormulae vars f
evalJEPFormulae vars (Arithmetic op f1 f2) =
  (case op of
     Divide -> (/)
     Multiply -> (*)
     Subtract -> (-)
     Add -> (+)
     GreaterEqualsThan -> \ x y -> if x >= y then 1 else 0) -- TODO check this
  (evalJEPFormulae vars f1)
  (evalJEPFormulae vars f2)
evalJEPFormulae vars (Function what formulas) =
  let f = builtInFunction what in
  let args = map (evalJEPFormulae vars) formulas in
  f args
evalJEPFormulae vars (Group f) = evalJEPFormulae vars f
evalJEPFormulae vars (Variable v) =
  case M.lookup v vars of
    Just n -> toRational n
    Nothing ->
      (warning $ "variable \"" ++ v ++ "\" was not found")
      0

parseRolls :: PParser [Roll]
parseRolls = parseRoll `sepBy` char '/'

parseSpecialRoll :: PParser Roll
parseSpecialRoll = Special <$ labeled "Special"

parseRollDie :: PParser Roll
parseRollDie = do
  number <- option 1 (textToInt <$> manyNumbers)
  _ <- optional $ char 'd'
  sides <- option 0 $ textToInt <$> manyNumbers
  modifier <- option 0 parseInteger -- account for e.g., 2d10+1 or 2d10-5
  return $ RollDie Die { .. }

parseRoll :: PParser Roll
parseRoll = parseSpecialRoll <|> parseRollDie

parseInteger :: PParser Int
parseInteger = parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (negate <$ char '-') <|> (id <$ optional (char '+'))

parseFloat :: PParser Float
parseFloat = parseSignedNumber where
  parseSignedNumber = sign <*> parseFloatOrInt
  sign = (negate <$ char '-') <|> (id <$ optional (char '+'))
  parseFloatOrInt = textToFloat <$> try (parseFloatingNumber <|> manyNumbers)
  parseFloatingNumber = do
    n <- option "" manyNumbers
    d <- char '.'
    r <- manyNumbers
    return $ n ++ d : r

parseNumber :: PParser Formula
parseNumber = Number <$> parseInteger

parseFloating :: PParser Formula
parseFloating = Floating <$> parseFloat

parseVariable :: PParser Formula
parseVariable = Variable <$> variableParsers

parseGroup :: PParser Formula
parseGroup = Group <$> (char '(' >> parseFormula <* char ')')

-- we treat all known unquoted variables specially (for now) -- not
-- sure what they refer to, just yet.
--
-- NB: may want to make sure there are no unterminated quotes!
parseQuotedString :: PParser String
parseQuotedString = labeled "ARMOR.0.ACCHECK"
                <|> labeled "SPELLSTAT"
                <|> char '"' *> untilQuote where
  untilQuote = manyTill anyChar $ satisfy (== '"')

-- since the var() function just allows the variable to contain
-- characters which are not valid JEP syntax, we just treat it as a
-- regular variable.
parseVarFunction :: PParser Formula
parseVarFunction = Variable <$> (labeled "var(" >> parseQuotedString <* labeled ")")

-- treat the skillinfo() function specially
parseSkillInfoFunction :: PParser Formula
parseSkillInfoFunction = do
  prop <- labeled "skillinfo(" *> parseQuotedString
  _ <- char ',' >> many space
  var <- parseQuotedString <* char ')'
  return $ LookupSkill (parseProperty prop, var) where
    parseProperty "RANK" = RANK
    parseProperty "TOTALRANK" = TOTALRANK
    parseProperty "MODIFIER" = MODIFIER
    parseProperty "STAT" = STAT
    parseProperty "MISC" = MISC
    parseProperty "TOTAL" = TOTAL
    parseProperty _ = error "No such skillinfo property"

parseFunction :: PParser Formula
parseFunction = do
  f <- choice functionParsers
  args <- char '(' >> parseFormula `sepBy` char ',' <* char ')'
  return $ Function f args

table :: [[Operator T.Text () (State Variables) Formula]]
table = [ [ Prefix negateFormula ]
        , [ Infix multiplyFormula AssocLeft, Infix divideFormula AssocLeft ]
        , [ Infix addFormula AssocLeft, Infix subtractFormula AssocLeft ]
        , [ Infix geFormula AssocLeft ]
        ] where
  multiplyFormula = operand "*" Multiply
  subtractFormula = operand "-" Subtract
  divideFormula = operand "/" Divide
  addFormula = operand "+" Add
  geFormula = operand ">=" GreaterEqualsThan
  operand c o = try $ Arithmetic o <$ labeled c
  negateFormula = try $ Negate <$ char '-'

parseFormula :: PParser Formula
parseFormula = buildExpressionParser table parseExpression
-- parseFormula = _traceFormula

parseExpression :: PParser Formula
parseExpression = try parseFunction
              <|> try parseGroup
              <|> try parseVarFunction
              <|> try parseSkillInfoFunction
              <|> try parseFloating
              <|> try parseNumber
              <|> try parseVariable

_traceFormula :: PParser Formula
_traceFormula = do
  v <- manyTill anyChar $ satisfy (\x -> x == '|' || x == '\t' || x == '\r' || x == '\n')
  _ <- trace ("** Formula was " ++ v) $ return ()
  return $ Variable v
