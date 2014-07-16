module Lst where

import Prelude hiding (takeWhile)
import qualified Text.Show.Pretty as Pretty
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim
import Control.Applicative hiding ((<|>), many)
import Modifications
import Common

-- custom lst types
import Lst.Skill(SkillDefinition)
import Lst.Language(LanguageDefinition)
import Lst.WeaponProf(WeaponProficency)
import Lst.ShieldProf(ShieldProficency)
import Lst.ArmorProf(ArmorProficency)

-- generic, catch-all
import Lst.Generic(LSTDefinition)

-- structure of a lst file
data LST a = Source [Header]
           | Definition a
           | Comment String deriving Show

-- source headers: these are found in nearly every lst file type.
data Header = SourceLong String
            | SourceShort String
            | SourceWeb String
            | SourceDate String deriving Show

parseSourceWeb :: Parser Header
parseSourceWeb = SourceWeb <$> (tag "SOURCEWEB" >> restOfTag)

parseSourceLong :: Parser Header
parseSourceLong = SourceLong <$> (tag "SOURCELONG" >> restOfTag)

parseSourceShort :: Parser Header
parseSourceShort = SourceShort <$> (tag "SOURCESHORT" >> restOfTag)

parseSourceDate :: Parser Header
parseSourceDate = SourceDate <$> (tag "SOURCEDATE" >> restOfTag)

parseHeaders :: Parser [Header]
parseHeaders = many1 header <* tabs where
  header = parseSourceLong
       <|> parseSourceShort
       <|> parseSourceWeb
       <|> parseSourceDate

parseLSTLines :: Parser a -> Parser [LST a]
parseLSTLines parseDefinition = do
  _ <- many eol
  many1 $ lstLine <* many eol where
    lstLine = Source <$> parseHeaders
          <|> Comment <$> parseCommentLine
          <|> Definition <$> parseDefinition

parseLST :: Show a => Parser (LSTLine a) -> FilePath -> IO [LST (LSTLine a)]
parseLST lstParser lstName  = do
  contents <- readContents lstName
  return . parseResult lstName $ parse fullParser lstName contents where
    fullParser = parseLSTLines lstParser

-- debugging only
prettyPrint :: Show a => Parser (LSTLine a) -> FilePath -> IO String
prettyPrint x file = Pretty.ppShow <$> parseLST x file

parseLSTToString :: String -> FilePath -> IO String
parseLSTToString "LANGUAGE" = prettyPrint (parseLSTLine :: Parser (LSTLine LanguageDefinition))
parseLSTToString "ARMORPROF" = prettyPrint (parseLSTLine :: Parser (LSTLine ArmorProficency))
parseLSTToString "SHIELDPROF" = prettyPrint (parseLSTLine :: Parser (LSTLine ShieldProficency))
parseLSTToString "WEAPONPROF" = prettyPrint (parseLSTLine :: Parser (LSTLine WeaponProficency))
parseLSTToString "SKILL" = prettyPrint (parseLSTLine :: Parser (LSTLine SkillDefinition))
parseLSTToString _ = prettyPrint (parseLSTLine :: Parser (LSTLine LSTDefinition))
