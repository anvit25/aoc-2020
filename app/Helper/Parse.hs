module Helper.Parse where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (toLower, toUpper)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as Char

type Parser = Parsec.Parsec String ()

type PError = Parsec.ParseError

parse :: Parser a -> String -> Either PError a
parse p = Parsec.parse p ""

anycharP :: [Char] -> Parser Char
anycharP = Char.oneOf

charP :: Char -> Parser Char
charP x = Char.oneOf [x]

stringP :: String -> Parser String
stringP = traverse charP

integer :: Parser Int
integer =
  read
    <$> ( (:)
            <$> (charP '-' <|> (' ' <$ charP '+') <|> return ' ')
            <*> Parsec.many1 Char.digit
        )

letter :: Parser Char
letter = Char.letter

multiple :: Parser a -> Parser [a]
multiple = Parsec.many1

enumP :: (Enum b, Show b, Bounded b) => Parser b
enumP = Parsec.choice $ Parsec.try . f <$> [minBound .. maxBound]
  where
    f a = a <$ insenStrP (show a)

insenCharP :: Char -> Parser Char
insenCharP c = anycharP [toLower c, toUpper c]

insenStrP :: String -> Parser String
insenStrP = traverse insenCharP

eof :: (Parsec.Stream s m t, Show t) => Parsec.ParsecT s u m ()
eof = Parsec.eof
