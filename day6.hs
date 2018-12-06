module DaySix where

import Data.List
import Data.Char

testInput :: [Pos]
testInput = [(1,1), (1,6), (8,3), (3,4), (5,5), (8,9)]

testOutput :: [Integer]
testOutput = [(-1), (-1), (-1), 9, 17, (-1)]

type Pos = (Int, Int)
data Coord = Coord Char Pos

makeCoords :: [String] -> [Pos]
makeCoords xs = map coordinate xs
  where
    parse (x:y:_) = ((read . init $ x :: Int), (read y :: Int))
    coordinate x = (parse . words) x




main :: IO ()
main = do
  -- input <- readFile "./data/daySixData.txt"
  -- let input = testInput
  -- let clean = lines . init $ input
  let solutionOne = testInput
  print solutionOne
