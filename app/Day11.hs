{-# LANGUAGE TupleSections #-}

-- module Day11 (day11a, day11b) where
module Day11 where

import Control.Applicative (liftA2)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as DS
import Helper.Misc (countIf)
import Text.Parsec (space)

data Piece
  = F -- (F)loor
  | E -- (E)mpty
  | O --  (O)ccupied
  deriving (Show, Eq)

type Position = (Int, Int)

type Board = M.Map Position Piece

day11a :: String -> Int
day11a =
  countIf (== O) . M.elems . fst . head
    . dropWhile (uncurry (/=))
    . liftA2 zip id tail
    . iterate nextBoard
    . boardP

day11b :: String -> Int
day11b =
  countIf (== O) . M.elems . fst . head
    . dropWhile (uncurry (/=))
    . liftA2 zip id tail
    . iterate nextBoard2
    . boardP

-- day11b = iterate nextBoard2 . boardP

getNewPiece :: [Piece] -> Piece -> Piece
getNewPiece xs E =
  if O `notElem` xs
    then O
    else E
getNewPiece xs O =
  if (> 3) $ countIf (== O) xs
    then E
    else O
getNewPiece _ F = F

getNewPiece2 :: [Piece] -> Piece -> Piece
getNewPiece2 xs E =
  if O `notElem` xs
    then O
    else E
getNewPiece2 xs O =
  if (> 4) $ countIf (== O) xs
    then E
    else O
getNewPiece2 _ F = F

nextBoard :: Board -> Board
nextBoard original = foldr helper M.empty $ M.keys original
  where
    helper :: Position -> Board -> Board
    helper pos newboard = M.insert pos (getNewPiece n $ original ! pos) newboard
      where
        n = getNeighbours original pos

nextBoard2 :: Board -> Board
nextBoard2 original = foldr helper M.empty $ M.keys original
  where
    helper :: Position -> Board -> Board
    helper pos newboard = M.insert pos (getNewPiece2 n $ original ! pos) newboard
      where
        n = getNeighbours2 original pos

getNeighbours :: Board -> Position -> [Piece]
getNeighbours b = mapMaybe (b !?) . neighbourPos
  where
    neighbourPos x =
      [ (xs, ys) + x
        | xs <- [-1 .. 1],
          ys <- [-1 .. 1],
          (xs, ys) /= (0, 0)
      ]

getNeighinDirec :: Board -> Position -> (Int, Int) -> Maybe Piece
getNeighinDirec b p d =
  if (b !? (p + d)) == Just F
    then getNeighinDirec b (p + d) d
    else b !? (p + d)

getNeighbours2 :: Board -> Position -> [Piece]
getNeighbours2 b p =
  mapMaybe (getNeighinDirec b p) $
    [ (xs, ys)
      | xs <- [-1 .. 1],
        ys <- [-1 .. 1],
        (xs, ys) /= (0, 0)
    ]

-- PARSING --

boardP :: String -> Board
boardP = fmap toPiece . M.fromList . concatMap modRow . rows
  where
    modRow :: (Int, String) -> [(Position, Char)]
    modRow (a, xs) = zip (map (a,) [0 ..]) xs
    rows = zip [0 ..] . lines

toPiece :: Char -> Piece
toPiece 'L' = E -- Empty
toPiece '#' = O -- Occupied
toPiece '.' = F -- Floor
toPiece _ = undefined
