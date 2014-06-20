{-# LANGUAGE OverloadedStrings #-}

module Lst.ShieldProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data ShieldProf = ShieldProf { shieldName :: T.Text } deriving Show

parseShieldProfLine :: Parser ShieldProf
parseShieldProfLine = do
  name <- parseString
  return ShieldProf { shieldName = name }
