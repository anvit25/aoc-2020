module Day3 where

import Data.Maybe (isJust)
import Helper.Misc (countIf, (!!?))

type Trajectory = (Int, Int)

type Pos = (Int, Int)

day3a :: String -> Int
day3a = countTrees (1, 3) . lines

day3b :: String -> Int
day3b xs =
  product . map ($ lines xs) $
    countTrees <$> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

countTrees :: Trajectory -> [String] -> Int
countTrees c xs =
  countIf (== Just '#') . takeWhile isJust
    . map (xs `twodlookup`)
    $ iterate (nextPos c cols) (0, 0)
  where
    cols = length $ head xs

nextPos :: Trajectory -> Int -> Pos -> Pos
nextPos (x, y) n (a, b) = (a + x, mod (b + y) n)

twodlookup :: [[a]] -> Pos -> Maybe a
twodlookup xs (a, b) = xs !!? a >>= (!!? b)
