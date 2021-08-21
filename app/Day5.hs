module Day5 (day5a, day5b) where

import Control.Applicative ((<|>))
import Data.List ((\\))
import Helper.Misc
import Helper.Parse
import Text.Parsec (count)

type Boarding = (Int, Int)

getseatIDs :: String -> Either PError [Int]
getseatIDs = mapM (parse (boardtoID <$> boardP)) . lines

day5a :: String -> Either PError Int
day5a = fmap maximum . getseatIDs

day5b :: String -> Either PError [Int]
day5b xs = do
  seatIds <- getseatIDs xs
  let l = minimum seatIds
  let u = maximum seatIds
  return $ [l .. u] \\ seatIds

rowP :: Parser Int
rowP = bintodec . reverse <$> count 7 fP
  where
    fP = '1' <$ charP 'B' <|> '0' <$ charP 'F'

colP :: Parser Int
colP = bintodec . reverse <$> count 3 fP
  where
    fP = '1' <$ charP 'R' <|> '0' <$ charP 'L'

boardP :: Parser Boarding
boardP = (,) <$> rowP <*> colP

boardtoID :: Boarding -> Int
boardtoID (a, b) = a * 8 + b
