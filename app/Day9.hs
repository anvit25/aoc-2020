module Day9 (day9a, day9b) where

import Control.Applicative (liftA2)
import Data.List (elemIndex, tails)
import Data.Maybe (fromJust, mapMaybe)
import Helper.Misc

preSize :: Int
preSize = 25

day9a :: String -> Int
day9a =
  (!! preSize) . find' (not . isValid)
    . tails
    . parseFile

day9b :: String -> Int
day9b xs =
  liftA2 (+) maximum minimum . firstJust' (findContiguous bad) $ possibles
  where
    bad = (!! preSize) . find' (not . isValid) $ possibles
    possibles = tails . parseFile $ xs

findContiguous :: Int -> [Int] -> Maybe [Int]
findContiguous x xs = do
  k <- x `elemIndex` partialsums
  return $ take (k + 1) xs
  where
    partialsums = scanl1 (+) xs

isValid :: [Int] -> Bool
isValid xs = (length ys < preSize) || finder y ys
  where
    ys = take preSize xs
    y = xs !! preSize

finder :: Int -> [Int] -> Bool
finder n [] = False
finder n (x : xs) = (n - x `elem` xs) || finder n xs
