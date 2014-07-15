{-# LANGUAGE OverloadedStrings #-}

module Lst.WeaponProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Restrictions
import Lst.GlobalTags
import Common

data WeaponProficency = Name T.Text
                      | WeaponType [T.Text]
                      | WeaponHands Int
                      | WeaponHandsRestriction Int
                      -- shared tags
                      | Global GlobalTag
                      | Restricted Restriction
                        deriving Show

parseWeaponType :: Parser WeaponProficency
parseWeaponType = WeaponType <$> (tag "TYPE" >> parseWordAndComma `sepBy` char '.') where
  -- comma only shows up in one file (apg_profs_weapon.lst). ugh.
  parseWordAndComma = takeWhile1 $ inClass "-A-Za-z, "

parseWeaponHands :: Parser WeaponProficency
parseWeaponHands = WeaponHands <$> (tag "HANDS" >> liftM textToInt manyNumbers)

parseWeaponHandsRestriction :: Parser WeaponProficency
parseWeaponHandsRestriction = do
  n <- tag "HANDS" *> manyNumbers
  _ <- string "IFLARGERTHANWEAPON"
  return . WeaponHandsRestriction $ textToInt n

parseWeaponProficencyTag :: Parser WeaponProficency
parseWeaponProficencyTag = parseWeaponType
                       <|> parseWeaponHandsRestriction
                       <|> parseWeaponHands
                       <|> Global <$> parseGlobalTags
                       <|> Restricted <$> parseRestriction

parseWeaponProficency :: T.Text -> Parser [WeaponProficency]
parseWeaponProficency weaponName = do
  weaponTags <- tabs *> parseWeaponProficencyTag `sepBy` tabs
  return $ weaponTags ++ [Name weaponName]

instance LSTObject WeaponProficency where
  parseLine = parseWeaponProficency
