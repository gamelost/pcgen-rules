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
                      | OutputName T.Text
                      | ProductIdentity Bool
                      | Restricted Restriction
                        deriving Show

parseProductIdentity :: Parser WeaponProficency
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

-- comma only shows up in one file (apg_profs_weapon.lst). ugh.
parseWordAndComma :: Parser T.Text
parseWordAndComma = takeWhile1 $ inClass "-A-Za-z, "

parseWeaponType :: Parser WeaponProficency
parseWeaponType = WeaponType <$> (tag "TYPE" >> parseWordAndComma `sepBy` char '.')

parseWeaponHands :: Parser WeaponProficency
parseWeaponHands = WeaponHands <$> (tag "HANDS" >> liftM textToInt manyNumbers)

parseOutputName :: Parser WeaponProficency
parseOutputName = OutputName <$> (tag "OUTPUTNAME" >> parseWordAndComma)

parseWeaponProficencyTag :: Parser WeaponProficency
parseWeaponProficencyTag = parseProductIdentity
                       <|> parseWeaponType
                       <|> parseWeaponHands
                       <|> parseOutputName
                       <|> Restricted <$> parseRestriction

parseWeaponProficency :: T.Text -> Parser [WeaponProficency]
parseWeaponProficency weaponName = do
  weaponTags <- tabs *> parseWeaponProficencyTag `sepBy` tabs
  return $ weaponTags ++ [Name weaponName]

instance LSTObject WeaponProficency where
  parseLine = parseWeaponProficency
