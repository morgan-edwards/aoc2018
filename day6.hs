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

data Direction = Northwest Pos
               | Northeast Pos
               | Southeast Pos
               | Southwest Pos
               | North Pos
               | East Pos
               | South Pos
               | West Pos
               | Colocate Pos
               deriving (Show)

type Pos = (Int, Int)
type Perimeter = ([Pos], [Pos], [Pos], [Pos])

makeCoords :: [String] -> [Pos]
makeCoords xs = map coordinate xs
  where
    parse (x:y:_) = ((read . init $ x :: Int), (read y :: Int))
    coordinate x = (parse . words) x

getDir :: Pos -> Pos -> Direction
getDir (centerX, centerY) (tarX, tarY)
  | tarY > centerY && tarX > centerX = Southeast (tarX, tarY)
  | tarY > centerY && tarX < centerX = Southwest (tarX, tarY)
  | tarY < centerY && tarX > centerX = Northeast (tarX, tarY)
  | tarY < centerY && tarX < centerX = Northwest (tarX, tarY)
  | tarY == centerY && tarX == centerX = Colocate (tarX, tarY)
  | tarY == centerY && tarX > centerX = East (tarX, tarY)
  | tarY == centerY && tarX < centerX = West (tarX, tarY)
  | tarY > centerY && tarX == centerX = South (tarX, tarY)
  | tarY < centerY && tarX == centerX = North (tarX, tarY)

distanceBetween :: Pos -> Pos -> Int
distanceBetween (x1, y1) (x2, y2) = distance
   where
    distance = abs (x1 - x2) + abs (y1 - y2)

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
  -- input <- readFile "./data/daySixData.txt"
  let input = testInput
  let clean = lines . init $ input
  let allCoords = makeCoords clean
  -- print allCoords
  print $ map (distanceBetween (5, 5)) [(8, 9)]
  -- print (map (getDir (allCoords !! 3)) allCoords)
