{-# LANGUAGE OverloadedStrings #-}

module Lst.ShieldProf where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Common

data ShieldProf = ShieldProf { shieldName :: T.Text } deriving Show

parseShieldProfLine :: Parser ShieldProf
parseShieldProfLine = do
  name <- parseString
  return ShieldProf { shieldName = name }
