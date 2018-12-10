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
data Procedure = Procedure { step :: Step,
                             preqs :: Prereqs
                           } deriving (Show)

processSteps :: [String] -> [(Step, Step)]
processSteps xs = map wordsToSteps xs
  where
    wordsToSteps x = let ws = words x in (head (ws !! 1), head (ws !! 7))

makeProcs :: [(Step, Step)] -> [Procedure]
makeProcs xs = foldl uniq [] $ map stepToProc xs
  where
    prereqs step = map fst $ filter (\x-> snd x == step) xs
    stepToProc x = let step = snd x in Procedure step (prereqs step)
    uniq acc (Procedure s prs)
      | elem s (map (step) acc) = acc
      | otherwise = (Procedure s prs):acc

allSteps :: [(Step, Step)] -> [Step]
allSteps xs = foldl aggSts [] xs
  where
    aggSts acc x
      | not (elem (fst x) acc) = fst x:acc
      | not (elem (snd x) acc) = snd x:acc
      | otherwise = acc

firstStep :: [Step] -> [Procedure] -> Step
firstStep ss ps = head . sort $ filter (not . hasPreReq ps) ss
  where
    hasPreReq ps s = elem s $ map (step) ps

possibleSteps :: [Step] -> [Procedure] -> [Step]
possibleSteps done remain = sort $ map (\(Procedure s prqs) -> s) $ filter (\x -> null . remainingPres $ x) remain
  where
    remainingPres (Procedure s prqs) = filter (\x -> not $ elem x done) prqs

-- get first step
-- get next step
-- remove the proc from the remaining list
-- when no procs remain, you're done

testSolutionOne = "CABDFE"

solutionOne :: [String] -> String
solutionOne xs = undefined

main :: IO ()
main = do
  -- input <- readFile "./data/daySevenData.txt"
  let input = testInput
  let clean = lines . init $ input
  let stages = processSteps clean
  let procs = makeProcs stages
  let steps = allSteps stages
  let done = "CA"
  let next = possibleSteps done procs
  print procs
  print next
  -- print $ solutionOne clean == testSolutionOne
