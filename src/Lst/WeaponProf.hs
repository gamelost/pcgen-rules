{-# LANGUAGE OverloadedStrings #-}

module Lst.WeaponProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data WeaponProf = WeaponProf { weaponName :: T.Text
                             , weaponType :: Maybe [T.Text]
                             , weaponHands :: Int
                             , productIdentity :: Bool -- optional
                             , modification :: Maybe Modification } deriving Show

parseProductIdentity :: Parser Bool
parseProductIdentity = string "NAMEISPI:" >> yesOrNo

parseWeaponType :: Parser [T.Text]
parseWeaponType = string "TYPE:" >> parseWord `sepBy` char '.'

parseWeaponHands :: Parser Int
parseWeaponHands = string "HANDS:" >> liftM textToInt manyNumbers

parseWeaponProficency :: Parser (T.Text, Maybe Modification) -> Parser WeaponProf
parseWeaponProficency p = do
  (name, modifier) <- p <* tabs
  pid <- option False parseProductIdentity <* tabs
  at <- optionMaybe parseWeaponType <* tabs
  hands <- option 1 parseWeaponHands
  return WeaponProf { weaponName = name
                    , weaponType = at
                    , weaponHands = hands
                    , productIdentity = pid
                    , modification = modifier }

parseWeaponProfLine :: Parser WeaponProf
parseWeaponProfLine = parseWeaponProficency parseStartMod <|>
                      parseWeaponProficency parseStart
