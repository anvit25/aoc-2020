module Day14 (day14a, day14b) where

import Data.Bits (clearBit, setBit)
import Data.Either (rights)
import qualified Data.IntMap as IM
import Data.Maybe (mapMaybe)
import Helper.Parse
import Text.Parsec (choice, noneOf, try)

data Command
  = Mask String
  | Mem (Int, Int)
  deriving (Show, Eq)

type Memory = ([Int -> Int], IM.IntMap Int)

type Memory2 = ([[Int -> Int]], IM.IntMap Int)

day14a :: String -> Int
day14a = sum . IM.elems . snd . foldl runCommand ([], IM.empty) . parseFile

day14b :: String -> Int
day14b = sum . IM.elems . snd . foldl runCommand2 ([], IM.empty) . parseFile

runCommand :: Memory -> Command -> Memory
runCommand (_, imap) (Mask mask) = (readMask mask, imap)
runCommand (mask, imap) (Mem (loc, val)) =
  (mask, IM.insert loc (foldr ($) val mask) imap)

readMask :: String -> [Int -> Int]
readMask = mapMaybe f . zip [0 ..] . reverse
  where
    f :: (Int, Char) -> Maybe (Int -> Int)
    f (i, '1') = Just (`setBit` i)
    f (i, '0') = Just (`clearBit` i)
    f (_, _) = Nothing

runCommand2 :: Memory2 -> Command -> Memory2
runCommand2 (_, imap) (Mask mask) = (readMask2 mask, imap)
runCommand2 (mask, imap) (Mem (loc, val)) =
  (mask, foldr (`IM.insert` val) imap locations)
  where
    locations = foldr ($) loc <$> mask :: [Int]

readMask2 :: String -> [[Int -> Int]]
readMask2 = foldl f [[]] . zip [0 ..] . reverse

f :: [[Int -> Int]] -> (Int, Char) -> [[Int -> Int]]
f xs (i, '1') = map ((`setBit` i) :) xs
f xs (i, 'X') = map ((`setBit` i) :) xs ++ map ((`clearBit` i) :) xs
f xs (_, _) = xs

commandP :: Parser Command
commandP = choice . map try $ [maskP, memP]

maskP :: Parser Command
maskP = Mask <$> (stringP "mask = " *> multiple (noneOf "\n"))

memP :: Parser Command
memP = do
  _ <- stringP "mem["
  i <- integer
  _ <- stringP "] = "
  j <- integer
  return $ Mem (i, j)

parseFile :: String -> [Command]
parseFile xs = rights . map (parse commandP) $ lines xs
