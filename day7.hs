module DaySeven where

import Data.List
import Data.Char

testInput :: String
testInput = "Step C must be finished before step A can begin.\n\
\Step C must be finished before step F can begin.\n\
\Step A must be finished before step B can begin.\n\
\Step A must be finished before step D can begin.\n\
\Step B must be finished before step E can begin.\n\
\Step D must be finished before step E can begin.\n\
\Step F must be finished before step E can begin.\n"

type Step = Char
type Prereqs = [Char]
data Procedure = Procedure Step Prereqs deriving (Show)

processSteps :: [String] -> [(Step, Step)]
processSteps (x:xs)
  | null xs = []
  | otherwise = getSteps x : processSteps xs
  where
    getSteps x = let ws = words x in (head (ws !! 1), head (ws !! 7))

makeProcedures :: [(Step, Step)] -> [Procedure]
makeProcedures xs = undefined

testSolutionOne :: String
testSolutionOne = "CABDFE"

solutionOne :: [String] -> String
solutionOne xs = undefined

main :: IO ()
main = do
  -- input <- readFile "./data/daySevenData.txt"
  let input = testInput
  let clean = lines . init $ input
  print . makeProcedures . processSteps $ clean
  -- print $ solutionOne clean == testSolutionOne
