module Lst.ShieldProf where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Restrictions
import Common

data ShieldProficency = Name T.Text
                      | Restricted Restriction
                        deriving Show

parseShieldProficencyTag :: Parser ShieldProficency
parseShieldProficencyTag = Restricted <$> parseRestriction

parseShieldProficency :: T.Text -> Parser [ShieldProficency]
parseShieldProficency shieldName = do
  shieldTags <- parseShieldProficencyTag `sepBy` tabs
  return $ shieldTags ++ [Name shieldName]

instance LSTObject ShieldProficency where
  parseLine = parseShieldProficency
