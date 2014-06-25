{-# LANGUAGE RecordWildCards #-}

module Lst.WeaponProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data WeaponProficency = WeaponProficency { weaponName :: T.Text
                                         , weaponType :: Maybe [T.Text]
                                         , weaponHands :: Int
                                         , productIdentity :: Bool } deriving Show

parseProductIdentity :: Parser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseWeaponType :: Parser [T.Text]
parseWeaponType = tag "TYPE" >> parseWord `sepBy` char '.'

parseWeaponHands :: Parser Int
parseWeaponHands = tag "HANDS" >> liftM textToInt manyNumbers

parseWeaponProficency :: T.Text -> Parser WeaponProficency
parseWeaponProficency weaponName = do
  productIdentity <- option False parseProductIdentity <* tabs
  weaponType <- optionMaybe parseWeaponType <* tabs
  weaponHands <- option 1 parseWeaponHands
  return WeaponProficency { .. }

instance LSTObject WeaponProficency where
  parseLine = parseWeaponProficency
