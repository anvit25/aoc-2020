module Day8 (day8a, day8b) where

import Data.Either (rights)
import Data.Maybe (mapMaybe)
import Data.Sequence ((!?))
import qualified Data.Sequence as DS
import Helper.Parse
import Text.Parsec (space)

data Command = NOP | ACC | JMP
  deriving (Show, Eq, Enum, Bounded)

type Ins = (Command, Int)

type Program = DS.Seq Ins

type State = (Int, Int) -- Position of pointer, Accumalator

day8a :: String -> Either PError (Either Int Int)
day8a xs = runProg <$> parse parseProg xs

day8b :: String -> Either PError Int
day8b xs = do
  prog <- parse parseProg xs
  return . head . rights . map runProg . newProgs $ prog

-- Compute the next position of pointer
-- Return Nothing in case current position
-- is outisde the given instructions
nextState :: Program -> State -> Maybe State
nextState m (curr, acc) = f <$> m !? curr
  where
    f y = case y of
      (ACC, x) -> (curr + 1, acc + x)
      (JMP, x) -> (curr + x, acc)
      (NOP, x) -> (curr + 1, acc)

-- Run the program till it either terminate or Repeats
-- Left corresponds to Repetition while
-- right corresponds to Terminations of program
runProg :: Program -> Either Int Int
runProg prog = runTillReporTerm prog visited (0, 0)
  where
    visited = DS.fromList $ take (length prog) [0, 0 ..]

-- Helper for runProg, run the program from a given state and
-- visited map
runTillReporTerm :: Program -> DS.Seq Int -> State -> Either Int Int
runTillReporTerm prog visited (curr, acc) = case newstate of
  Nothing -> Right acc
  Just state ->
    if DS.index visited curr > 0
      then Left acc
      else runTillReporTerm prog newVisited state
  where
    newstate = nextState prog (curr, acc)
    newVisited = DS.adjust (+ 1) curr visited

-- Creates a list of new programs, changing JMP
-- to NOP and vice verse. Each new program is exactly
-- one command different from the original program
newProgs :: Program -> [Program]
newProgs prog = mapMaybe (modify prog) [0 .. length prog - 1]
  where
    modify :: Program -> Int -> Maybe Program
    modify prog i = case DS.index prog i of
      (ACC, x) -> Nothing
      (JMP, x) -> Just $ DS.update i (NOP, x) prog
      (NOP, x) -> Just $ DS.update i (JMP, x) prog

parseIns :: Parser Ins
parseIns = (,) <$> (enumP <* space) <*> (integer <* space)

parseProg :: Parser Program
parseProg = DS.fromList <$> multiple parseIns <* eof
