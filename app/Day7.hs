module Day7 (day7a, day7b) where

import Control.Applicative ((<|>))
import Data.Function.Memoize (memoize)
import Data.Map.Strict (Map, empty, fromList, keys, (!), (!?))
import Helper.Parse
import Text.Parsec (many, sepBy)

type Bag = (String, String)

type Bags = Map Bag (Map Bag Int)

day7a :: String -> Either PError Int
day7a xs = do
  bags <- parse parsefull xs
  let memconShinyGold = memoize $ containsShinyGold bags
  let possible = keys bags
  return . length . filter memconShinyGold $ possible

day7b :: String -> Either PError Int
day7b xs = do
  bags <- parse parsefull xs
  return . countShiny bags $ ("shiny", "gold")

countShiny :: Bags -> Bag -> Int
countShiny bags x =
  if null contains
    then 0
    else sum $ map (\x -> contains ! x * (1 + countShiny bags x)) possible
  where
    contains = bags ! x
    possible = keys contains

containsShinyGold :: Bags -> Bag -> Bool
containsShinyGold bags bag = case contains !? ("shiny", "gold") of
  Nothing ->
    if null contains
      then False
      else any (containsShinyGold bags) $ keys contains
  Just _ -> True
  where
    contains = bags ! bag

parsefull :: Parser Bags
parsefull = fromList <$> multiple parseLine

parseLine :: Parser (Bag, Map Bag Int)
parseLine = do
  adj <- multiple letter
  charP ' '
  col <- multiple letter
  stringP " bags contain "
  ma <- emptybag <|> nonemptybag
  charP '\n'
  return ((adj, col), ma)

emptybag :: Parser (Map Bag Int)
emptybag = empty <$ stringP "no other bags."

nonemptybag :: Parser (Map Bag Int)
nonemptybag = fromList <$> sepBy containsP (stringP ", ") <* charP '.'

containsP :: Parser (Bag, Int)
containsP = do
  n <- integer <* charP ' '
  adj <- multiple letter <* charP ' '
  col <- multiple letter
  stringP " bag" *> many (charP 's')
  return ((adj, col), n)
