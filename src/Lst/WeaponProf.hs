{-# LANGUAGE OverloadedStrings #-}

module Lst.WeaponProf where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, sepBy1, many1)
import Text.Parsec.Prim (try)
import ClassyPrelude hiding (try)

import Modifications
import Restrictions
import Lst.GlobalTags
import Clear(parseClear, ClearTag(..))
import Bonus(parseBonus, Bonus)
import Common

data WeaponProficency = Name String
                      | WeaponType [String]
                      | WeaponHands Int
                      | WeaponHandsRestriction Int
                      -- shared tags
                      | WeaponBonus Bonus
                      | WeaponClear ClearTag
                      | Global GlobalTag
                      | Restricted Restriction
                        deriving Show

parseWeaponType :: PParser WeaponProficency
parseWeaponType = WeaponType <$> (tag "TYPE" >> parseWordAndComma `sepBy1` char '.') where
  -- comma only shows up in one file (apg_profs_weapon.lst). ugh.
  parseWordAndComma = many1 $ satisfy $ inClass "-A-Za-z, "

parseWeaponHands :: PParser WeaponProficency
parseWeaponHands = WeaponHands <$> (tag "HANDS" >> liftM textToInt manyNumbers)

parseWeaponHandsRestriction :: PParser WeaponProficency
parseWeaponHandsRestriction = do
  n <- tag "HANDS" *> manyNumbers
  _ <- labeled "IFLARGERTHANWEAPON"
  return . WeaponHandsRestriction $ textToInt n

parseWeaponProficencyTag :: PParser WeaponProficency
parseWeaponProficencyTag = WeaponClear <$> parseClear
                       <|> parseWeaponType
                       <|> try parseWeaponHandsRestriction
                       <|> try parseWeaponHands
                       <|> WeaponBonus <$> parseBonus
                       <|> Restricted <$> parseRestriction
                       <|> Global <$> parseGlobalTags

parseWeaponProficency :: String -> PParser [WeaponProficency]
parseWeaponProficency weaponName = do
  weaponTags <- tabs *> parseWeaponProficencyTag `sepBy` tabs
  return $ weaponTags ++ [Name weaponName]

instance LSTObject WeaponProficency where
  parseLine = parseWeaponProficency
