module Clear ( ClearTag(..)
             , parseClear
             ) where

import Text.Parsec.Combinator (choice)
import Data.Maybe (fromJust)
import ClassyPrelude

import Common

data ClearTag = Clear String
              | ClearAll String
              deriving (Show, Eq)

parseClear :: PParser ClearTag
parseClear = ClearAll <$> parseClearAll
         <|> Clear <$> parseClearOnly

parseClearOnly :: PParser String
parseClearOnly = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "RANGE"
                               , "TYPE"
                               , "PRE"
                               , "DESC"
                               , "CSKILL"
                               , "AUTO"
                               , "LANG"
                               , "TARGETAREA"
                               , "SCHOOL"
                               , "SAB"]
  return $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEAR")
    stripClear = fromJust . stripSuffix ":.CLEAR"

parseClearAll :: PParser String
parseClearAll = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "CLASSES"
                               , "DESC" ]
  return $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEARALL")
    stripClear = fromJust . stripSuffix ":.CLEARALL"
