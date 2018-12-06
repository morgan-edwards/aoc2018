module DayFive where

import Data.List
import Data.Char

type Pair = (Char, Char)
type UnitReduction = (Char, Int)

react :: Pair -> Bool
react (x, y) = (abs $ ord x - ord y) == 32

chainReaction :: String -> String
chainReaction xs = go "" xs
  where
    go i chain
      | null chain = i
      | null i = go ([head chain]) (tail chain)
      | reacted = go (init i) (tail chain)
      | not reacted = go (i ++ [head chain]) (tail chain)
      where
        reacted = react (last i, head chain)

analyzeRemoval :: String -> Char -> UnitReduction
analyzeRemoval xs c = (c, length reacted)
  where
    removed = filter (\x -> x /= toLower c && x /= toUpper c) xs
    reacted = chainReaction removed

sortReductions :: UnitReduction -> UnitReduction -> Ordering
sortReductions (ch1, r1) (ch2, r2)
  | r1 > r2 = GT
  | r1 < r2 = LT
  | r1 == r2 = EQ

improvePolymer :: String -> UnitReduction
improvePolymer xs = head $ sortBy sortReductions (map (analyzeRemoval xs) ['a'..'z'])

main :: IO ()
main = do
  input <- readFile "./data/dayFiveData.txt"
  let clean = filter (/= '\n') input
  let solutionOne = length $ chainReaction clean
  let solutionTwo = improvePolymer clean
  print solutionOne
  print solutionTwo
