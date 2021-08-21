module Day9 (day9a, day9b) where

import Data.List (tails, elemIndex)
import Data.Maybe (mapMaybe)

day9a :: String -> Int
day9a = (!! preamble) . head . 
            filter (not . isValid) . 
            tails . map read . lines

day9b :: String -> Int
day9b xs =
  (\x -> maximum x + minimum x) . head . mapMaybe (findContiguous bad) $ possibles
 where
  bad       = (!! preamble) . head . filter (not . isValid) $ possibles
  possibles = tails . map read . lines $ xs


findContiguous :: Int -> [Int] -> Maybe [Int]
findContiguous x xs = do
  k <- x `elemIndex` partialsums
  return $ take (k + 1) xs
  where partialsums = scanl1 (+) xs


preamble :: Int
preamble = 25

isValid :: [Int] -> Bool
isValid xs = (length ys < preamble) || (finder y ys)
    where ys = take preamble xs
          y  = xs !! preamble

finder :: Int -> [Int] -> Bool
finder n []       = False
finder n (x : xs) = (n - x `elem` xs) || finder n xs