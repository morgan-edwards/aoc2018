module DayThree where

import Data.List

type Fabric = [[Point]]
type Id = String

data Status = Free | Con | Cut String deriving (Show)
data Point = Point Int Int Status deriving (Show)
data Shape = Shape Point Point deriving (Show)
data Plan = Plan Id Shape
--
-- makePlan :: String -> Plan
-- makePlan code = let
--   elfId = takeWhile (\x -> x \= ' ') code
--
--   in expression

makeRow :: Int -> [Int] -> [Point]
makeRow x ys = map (makePoint x) ys
  where
    makePoint = (\x y -> Point x y Free)

fullFabric :: Shape -> Fabric
fullFabric (Shape (Point x1 y1 _) (Point x2 y2 _)) = go xRange yRange []
  where
    xRange = [x1..x2]
    yRange = [y1..y2]
    go range1 range2 acc
      | length range1 == 0 = acc
      | otherwise = go (tail range1) (range2) (acc ++ [makeRow (head range1) range2])

cutPoint :: Point -> [Fabric]
cutPoint aim = undefined


main :: IO ()
main = do
  -- input <- readFile "./data/dayThreeData.txt"
  -- let dataPoints = map words $ lines input]
  let start = Point 0 0 Free
  let end = Point 10 10 Free
  let cloth = Shape start end
  let output = fullFabric cloth
  -- let output = justYs $ fullFabric cloth
  print output
