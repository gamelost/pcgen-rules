{-# LANGUAGE OverloadedStrings #-}

module Lst.WeaponProf where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy1, many1)
import Text.Parsec.Prim (try)
import ClassyPrelude hiding (try)

import Modifications
import Common

data WeaponProficency = WeaponType [String]
                      | WeaponHands Int
                      | WeaponHandsRestriction Int
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
parseWeaponProficencyTag = parseWeaponType
                       <|> try parseWeaponHandsRestriction
                       <|> try parseWeaponHands

instance LSTObject WeaponProficency where
  parseSpecificTags = parseWeaponProficencyTag
