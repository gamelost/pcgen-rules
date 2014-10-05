module Clear ( ClearTag(..)
             , parseClear
             ) where

import Common
import Text.Parsec.Combinator
import Data.Maybe

data ClearTag = Clear String
              deriving (Show, Eq)

parseClear :: PParser ClearTag
parseClear = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "RANGE"
                               , "TYPE"
                               , "PRE"
                               , "DESC"
                               , "CSKILL"
                               , "AUTO"
                               , "LANG"
                               , "SAB"]
  return $ Clear $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEAR")
    stripClear = fromJust . stripSuffix ":.CLEAR"
