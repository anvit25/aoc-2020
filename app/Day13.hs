module Day13 (day13a, day13b) where

import Control.Applicative (Applicative (liftA2))
import Data.Either (fromRight, rights)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Helper.Parse
import Text.Parsec (sepBy, (<|>))

day13a :: String -> Int
day13a xs = liftA2 (*) fst snd . minimumBy (comparing snd) . zip ids $ zipWith (-) ids mods
  where
    [l1, l2] = lines xs
    early = read l1 :: Int
    ids = rights . fromRight [] $ parse myParser l2
    mods = map (mod early) ids

day13b :: String -> Int
day13b xs = fst $ foldl chineseRT (0, 1) modTups
  where
    [_, l2] = lines xs
    modTups = fmap (\(x, y) -> ((- x) `mod` y, y)) . rights . zipWith (fmap . (,)) [0 ..] $ ids
    ids = fromRight [] $ parse myParser l2

myParser :: Parser [Either Char Int]
myParser = sepBy f $ stringP ","
  where
    f = Left <$> charP 'x' <|> Right <$> integer

chineseRT ::
  (Int, Int) -> -- (current value, increments)
  (Int, Int) -> -- (mod value, n)
  (Int, Int) --    (new value, increments)
chineseRT (c, i) (m, n) = (new_c, new_i)
  where
    new_i = lcm i n
    new_c = head $ filter ((== m) . (`mod` n)) [c, c + i ..]
