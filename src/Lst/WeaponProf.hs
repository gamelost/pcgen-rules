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
                             , productIdentity :: Bool } deriving Show

type WeaponMod = Modification WeaponProf

parseProductIdentity :: Parser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseWeaponType :: Parser [T.Text]
parseWeaponType = tag "TYPE" >> parseWord `sepBy` char '.'

parseWeaponHands :: Parser Int
parseWeaponHands = tag "HANDS" >> liftM textToInt manyNumbers

parseWeaponProficency :: Parser (T.Text, Operation) -> Parser WeaponMod
parseWeaponProficency p = do
  (name, op) <- p <* tabs
  pid <- option False parseProductIdentity <* tabs
  at <- optionMaybe parseWeaponType <* tabs
  hands <- option 1 parseWeaponHands
  return Modification { operation = op
                      , record = WeaponProf { weaponName = name
                                            , weaponType = at
                                            , weaponHands = hands
                                            , productIdentity = pid } }

parseWeaponLine :: Parser WeaponMod
parseWeaponLine = parseWeaponProficency parseMod <|>
                  parseWeaponProficency parseAdd
