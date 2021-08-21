module Day8 (day8a, day8b) where

import Helper.Parse
import qualified Data.IntMap as IM
import Data.IntMap ((!), (!?))
import Data.Maybe ( mapMaybe ) 
import Data.Either (isRight)

data Command = NOP | ACC | JMP
    deriving (Show, Eq, Enum, Bounded)
type Ins = (Command, Int)
type Program = IM.IntMap Ins
type State = 
    (Int, -- Position of pointer
     Int) -- Accumalator


day8a :: String -> Either PError (Either Int Int)
day8a = (runProg <$>) . parse parseProg

day8b :: String -> Either PError [Either Int Int]
day8b xs = do
    prog <-  parse parseProg xs
    let progs = newProgs prog
    return . filter isRight . map runProg $ progs

parseIns :: Parser Ins
parseIns = do
    com <- enumP
    charP ' '
    i <- integer
    charP '\n'
    return (com, i)

parseProg :: Parser Program
parseProg = IM.fromList . zip [0..] <$> multiple parseIns <* eof

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
runProg prog = runTillReporTerm prog visited (0,0)
    where visited = IM.fromList $ zip [0.. length prog - 1] [0,0..]

-- Helper for runProg, run the program from a given state and 
-- visited map
runTillReporTerm :: Program -> IM.IntMap Int -> State -> Either Int Int
runTillReporTerm prog visited (curr, acc) = case newstate of
    Nothing -> Right acc
    Just state -> if visited ! curr > 0
        then Left acc 
        else runTillReporTerm prog newVisited state
  where
    newstate   = nextState prog (curr, acc)
    newVisited = IM.adjust (+ 1) curr visited


-- Creates a list of new programs, changing JMP
-- to NOP and vice verse. Each new program is exactly
-- one command different from the original program
newProgs :: Program -> [Program]
newProgs prog = mapMaybe (modify prog) [0 .. length prog - 1]
    where modify :: Program -> Int -> Maybe Program
          modify prog i = case prog ! i of 
              (ACC, x) -> Nothing
              (JMP, x) -> Just $ IM.insert i (NOP, x) prog 
              (NOP, x) -> Just $ IM.insert i (JMP, x) prog
