{-# LANGUAGE RecordWildCards #-}

module Lst.ShieldProf where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Modifications
import Common

data ShieldProficency = ShieldProficency { shieldName :: T.Text } deriving Show

parseShieldProficency :: T.Text -> Parser ShieldProficency
parseShieldProficency shieldName = return ShieldProficency { .. }

instance LSTObject ShieldProficency where
  parseLine = parseShieldProficency
