module Lst where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
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
           | Comment T.Text deriving Show

-- source headers: these are found in nearly every lst file type.
data Header = SourceLong T.Text
            | SourceShort T.Text
            | SourceWeb T.Text
            | SourceDate T.Text deriving Show

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
  _ <- many endOfLine
  many1 $ lstLine <* many endOfLine where
    lstLine = Source <$> parseHeaders
          <|> Comment <$> parseCommentLine
          <|> Definition <$> parseDefinition

parseLST :: Parser (LSTLine a) -> FilePath -> IO [LST (LSTLine a)]
parseLST lstParser lstName  = do
  contents <- readContents lstName
  return . parseResult lstName $ parse fullParser contents where
    fullParser = parseLSTLines lstParser

parseLanguageLST = parseLST (parseLSTLine :: Parser (LSTLine LanguageDefinition))
parseWeaponLST = parseLST (parseLSTLine :: Parser (LSTLine WeaponProficency))
parseShieldLST = parseLST (parseLSTLine :: Parser (LSTLine ShieldProficency))
parseArmorLST = parseLST (parseLSTLine :: Parser (LSTLine ArmorProficency))
parseSkillLST = parseLST (parseLSTLine :: Parser (LSTLine SkillDefinition))
parseGenericLST = parseLST (parseLSTLine :: Parser (LSTLine LSTDefinition))
