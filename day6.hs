module DaySix where

import Data.List
import Data.Char

testInput :: String
testInput = "1, 1\n\
\1, 6\n\
\8, 3\n\
\3, 4\n\
\5, 5\n\
\8, 9\n"

testOutput :: [Integer]
testOutput = [(-1), (-1), (-1), 9, 17, (-1)]

type Pos = (Int, Int)
type Perimeter = (Maybe Pos, Maybe Pos, Maybe Pos, Maybe Pos)

makeCoords :: [String] -> [Pos]
makeCoords xs = map coordinate xs
  where
    parse (x:y:_) = ((read . init $ x :: Int), (read y :: Int))
    coordinate x = (parse . words) x

perimeter :: [Pos] -> Pos -> (Pos, Perimeter)
perimeter allPos center = (center, (ne, nw, sw, se))
  where
    ne = Just center
    nw = Just center
    sw = Just center
    se = Just center

areaBetween :: Pos -> Maybe Pos -> Integer
areaBetween x y = 3

surroundingArea :: (Pos, Perimeter) -> Integer
surroundingArea (center, (ne, nw, sw, se))
  | not . null . filter (\x -> x == Nothing) $ [ne, nw, sw, se] = (-1)
  | otherwise = sum $ map (areaBetween center) [ne, nw, sw, se]


partOne :: [String] -> [Integer]
partOne xs = map surroundingArea . map (perimeter coords) $ coords
  where
    coords = makeCoords xs



main :: IO ()
main = do
  -- input <- readFile "./data/daySixData.txt"
  let input = testInput
  let clean = lines . init $ input
  let solutionOne = partOne clean
  print solutionOne
