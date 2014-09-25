{-# LANGUAGE OverloadedStrings #-}

module Lst.WeaponProf where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Control.Monad(liftM)
import Control.Applicative
import Modifications
import Restrictions
import Lst.GlobalTags
import Common
import Bonus(parseBonus, Bonus)

data WeaponProficency = Name String
                      | WeaponType [String]
                      | WeaponHands Int
                      | WeaponHandsRestriction Int
                      -- shared tags
                      | WeaponBonus Bonus
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
parseWeaponProficencyTag = parseWeaponType
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
