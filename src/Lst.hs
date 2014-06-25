module Lst where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Modifications
import Common

-- custom lst types
-- import Lst.Skill(SkillDefinition)
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
parseSourceWeb = SourceWeb <$> (tag "SOURCEWEB" >> parseUrl)

parseSourceLong :: Parser Header
parseSourceLong = SourceLong <$> (tag "SOURCELONG" >> parseString)

parseSourceShort :: Parser Header
parseSourceShort = SourceShort <$> (tag "SOURCESHORT" >> parseString)

parseSourceDate :: Parser Header
parseSourceDate = SourceDate <$> (tag "SOURCEDATE" >> parseString)

parseHeaders :: Parser [Header]
parseHeaders = many1 (parseSourceLong <|>
                      parseSourceShort <|>
                      parseSourceWeb <|>
                      parseSourceDate) <* tabs

parseLSTLines :: Parser a -> Parser [LST a]
parseLSTLines parseDefinition = do
  _ <- many endOfLine
  many1 $ (liftM Source parseHeaders <|>
           liftM Comment parseCommentLine <|>
           liftM Definition parseDefinition) <* many endOfLine

parseLST :: Parser (LSTLine a) -> FilePath -> IO [LST (LSTLine a)]
parseLST lstParser lstName  = do
  contents <- readContents lstName
  return . parseResult lstName $ parse (parseLSTLines lstParser) contents

parseLanguageLST = parseLST (parseLSTLine :: Parser (LSTLine LanguageDefinition))
parseWeaponLST = parseLST (parseLSTLine :: Parser (LSTLine WeaponProficency))
parseShieldLST = parseLST (parseLSTLine :: Parser (LSTLine ShieldProficency))
parseArmorLST = parseLST (parseLSTLine :: Parser (LSTLine ArmorProficency))
-- parseSkillLST = parseLST (parseLSTLine :: Parser (LSTLine SkillProficency))
parseGenericLST = parseLST (parseLSTLine :: Parser (LSTLine LSTDefinition))
