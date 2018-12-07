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

data Direction = North | East | South | West deriving (Show)

type Pos = (Int, Int)
type Perimeter = (Pos, Pos, Pos, Pos)

makeCoords :: [String] -> [Pos]
makeCoords xs = map coordinate xs
  where
    parse (x:y:_) = ((read . init $ x :: Int), (read y :: Int))
    coordinate x = (parse . words) x

directionSort :: Direction -> Pos -> Pos -> Ordering
directionSort d (x1, y1) (x2, y2)
  | c1 > c2 = GT
  | c1 < c2 = LT
  | c1 == c2 = EQ
  where
    (c1, c2) = case d of
      North -> (y1, y2)
      South -> (y2, y1)
      West -> (x1, x2)
      East -> (x2, x1)

furthest :: Direction -> [Pos] -> (Pos, Pos)
furthest d ps = (head sorted, last sorted)
  where
    sorted = sortBy (directionSort d) ps

perimeter :: [Pos] -> Perimeter
perimeter ps = (fst ns, snd we, snd ns, fst we)
  where
    ns = furthest North ps
    we = furthest West ps

distanceBetween :: Pos -> Pos -> Int
distanceBetween (x1, y1) (x2, y2) = distance
   where
    distance = abs (x1 - x2) + abs (y1 - y2)

-- interiorPoints :: Perimeter -> (Int, Int, Int, Int)
-- interiorPoints ((_, n), (e, _), (_, s), (w, _)) = (n, e, s, w)
interiorPoints :: Perimeter -> [Pos] -> [Pos]
interiorPoints ((_, n), (e, _), (_, s), (w, _)) ps = [ p | p <- ps, fst p < e && fst p > w && snd p > n && snd p < s]


-- surroundingArea :: (Pos, Perimeter) -> Integer
-- surroundingArea (center, (ne, nw, sw, se))
--   | not . null . filter (\x -> x == Nothing) $ [ne, nw, sw, se] = (-1)
--   | otherwise = sum $ map (areaBetween center) [ne, nw, sw, se]


-- partOne :: [String] -> [Integer]
-- partOne xs = map surroundingArea . map (perimeter coords) $ coords
--   where
--     coords = makeCoords xs



main :: IO ()
main = do
  input <- readFile "./data/daySixData.txt"
  -- let input = testInput
  let clean = lines . init $ input
  let allCoords = makeCoords clean
  let outer = perimeter allCoords
  let finitePoints = interiorPoints outer allCoords
  print outer
  print . length $ finitePoints
  -- print (map (getDir (allCoords !! 3)) allCoords)
