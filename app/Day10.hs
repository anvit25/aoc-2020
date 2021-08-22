module Day10 (day10a, day10b) where

import Data.Function.Memoize (memoize)
import Data.List (sort, tails)
import Helper.Misc

day10a :: String -> Int
day10a xs = (*) nThrees nOnes
  where
    adapters = (0 :) . sort . map read $ lines xs
    differences = zipWith (-) (tail adapters) adapters
    nThrees = (+ 1) . length $ filter (== 3) differences
    nOnes = length $ filter (== 1) differences

day10b :: String -> Int
day10b = count . reverse . (0 :) . sort . map read . lines

count :: [Int] -> Int
count [] = 0
count [_] = 1
count [_, _] = 1
count (x : xs) =
  sum . map count'
    . takeWhile ((\y -> x - y < 4) . head)
    . takeWhile (not . null)
    $ tails xs

count' :: [Int] -> Int
count' = memoize count
