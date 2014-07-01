module Lst.WeaponProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Restrictions
import Common

data WeaponProficency = Name T.Text
                      | WeaponType [T.Text]
                      | WeaponHands Int
                      | ProductIdentity Bool
                      | Restricted Restriction
                        deriving Show

parseProductIdentity :: Parser WeaponProficency
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

parseWeaponType :: Parser WeaponProficency
parseWeaponType = WeaponType <$> (tag "TYPE" >> parseWord `sepBy` char '.')

parseWeaponHands :: Parser WeaponProficency
parseWeaponHands = WeaponHands <$> (tag "HANDS" >> liftM textToInt manyNumbers)

parseWeaponProficencyTag :: Parser WeaponProficency
parseWeaponProficencyTag = parseProductIdentity <|>
                           parseWeaponType <|>
                           parseWeaponHands <|>
                           Restricted <$> parseRestriction

parseWeaponProficency :: T.Text -> Parser [WeaponProficency]
parseWeaponProficency weaponName = do
  weaponTags <- tabs *> parseWeaponProficencyTag `sepBy` tabs
  return $ weaponTags ++ [Name weaponName]

instance LSTObject WeaponProficency where
  parseLine = parseWeaponProficency
