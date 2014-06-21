module Lst.ShieldProf where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Modifications
import Common

data ShieldProf = ShieldProf { shieldName :: T.Text } deriving Show

type ShieldMod = Modification ShieldProf

parseShieldLine :: Parser ShieldMod
parseShieldLine = do
  name <- parseString
  return Modification { operation = Add
                      , record = ShieldProf { shieldName = name } }
