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
parseProductIdentity = do
  _ <- string "NAMEISPI:"
  answer <- allCaps
  return $ answer == "YES"

parseWeaponProfMod :: Parser (T.Text, Maybe Modification)
parseWeaponProfMod = liftM (\x -> (x, Just Add)) parseModification

parseWeaponProf :: Parser (T.Text, Maybe Modification)
parseWeaponProf = liftM (\x -> (x, Nothing)) parseString

parseWeaponType :: Parser [T.Text]
parseWeaponType = do
  _ <- string "TYPE:"
  parseWord `sepBy` char '.'

parseWeaponHands :: Parser Int
parseWeaponHands = do
  _ <- string "HANDS:"
  n <- manyNumbers
  return $ textToInt n

parseWeaponProficency :: Parser (T.Text, Maybe Modification) -> Parser WeaponProf
parseWeaponProficency p = do
  (name, modifier) <- p <* tabs
  pid <- option False parseProductIdentity <* tabs
  at <- option Nothing $ liftM Just parseWeaponType <* tabs
  hands <- option 1 parseWeaponHands
  return WeaponProf { weaponName = name
                    , weaponType = at
                    , weaponHands = hands
                    , productIdentity = pid
                    , modification = modifier }

parseWeaponProfLine :: Parser WeaponProf
parseWeaponProfLine = parseWeaponProficency parseWeaponProfMod <|>
                      parseWeaponProficency parseWeaponProf
