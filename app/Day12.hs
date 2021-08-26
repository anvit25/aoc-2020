module Day12 (day12a, day12b) where

import Data.Either (rights)
import Helper.Misc
import Helper.Parse

data Action
  = N
  | S
  | E
  | W
  | L
  | R
  | F
  deriving (Eq, Enum, Show, Bounded)

type Command = (Action, Int)

type Position = (Action, Int, Int)

type Waypoint = (Int, Int)

type PosWithWay = (Position, Waypoint)

day12a :: String -> Int
day12a = (\(_, y, z) -> abs y + abs z) . foldl doCommand (E, 0, 0) . parseFull

day12b :: String -> Int
day12b = (\(_, y, z) -> abs y + abs z) . fst . foldl doCommand2 ((E, 0, 0), (10, 1)) . parseFull

doCommand :: Position -> Command -> Position
doCommand (d, x, y) (N, i) = (d, x, y + i)
doCommand (d, x, y) (E, i) = (d, x + i, y)
doCommand (d, x, y) (S, i) = (d, x, y - i)
doCommand (d, x, y) (W, i) = (d, x - i, y)
doCommand (d, x, y) (F, i) = doCommand (d, x, y) (d, i)
doCommand (d, x, y) (L, i) = rotateClock90 (- i `div` 90) (d, x, y)
doCommand (d, x, y) (R, i) = rotateClock90 (i `div` 90) (d, x, y)

doCommand2 :: PosWithWay -> Command -> PosWithWay
doCommand2 ((sd, sx, sy), (wx, wy)) (F, i) = ((sd, sx + i * wx, sy + i * wy), (wx, wy))
doCommand2 (s, w) (L, i) = rotateWaypoint (- i `div` 90) (s, w)
doCommand2 (s, w) (R, i) = rotateWaypoint (i `div` 90) (s, w)
doCommand2 (s, (wx, wy)) (N, i) = (s, (wx, wy + i))
doCommand2 (s, (wx, wy)) (S, i) = (s, (wx, wy - i))
doCommand2 (s, (wx, wy)) (E, i) = (s, (wx + i, wy))
doCommand2 (s, (wx, wy)) (W, i) = (s, (wx - i, wy))

rotateClock90 :: Int -> Position -> Position
rotateClock90 0 x = x
rotateClock90 n (d, x, y) =
  if not $ isBetween 0 3 n
    then rotateClock90 (mod n 4) (d, x, y)
    else case d of
      N -> rotateClock90 (n - 1) (E, x, y)
      E -> rotateClock90 (n - 1) (S, x, y)
      S -> rotateClock90 (n - 1) (W, x, y)
      W -> rotateClock90 (n - 1) (N, x, y)
      _ -> error "Bruh Direction?"

rotateWaypoint :: Int -> PosWithWay -> PosWithWay
rotateWaypoint 0 (s, w) = (s, w)
rotateWaypoint n (s, (wx, wy)) = rotateWaypoint (n `mod` 4 - 1) (s, (wy, - wx))

parseLine :: Parser Command
parseLine = do
  a <- enumP
  i <- integer
  return (a, i)

parseFull :: String -> [Command]
parseFull = rights . map (parse parseLine) . lines
