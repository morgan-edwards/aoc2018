{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (lookup)
import Data.List (sort, group, foldl')
import Data.Map.Strict (Map, assocs, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

newtype GuardId = GuardId
  { unGuardId :: Int }
  deriving (Ord, Eq, Show)

newtype Minute = Minute
  { unMinute :: Int }
  deriving (Ord, Eq, Enum, Num, Show)

data SleepState = Awake
                | Asleep
                deriving (Ord, Eq, Show)

newtype GuardStates = GuardState
  { unGuardStates :: [(SleepState, Minute)] }
  deriving (Ord, Eq, Semigroup, Monoid, Show)

data State = State
  { _state_guardId :: GuardId
  , _state_sleepState :: SleepState
  , _state_minute :: Minute
  } deriving Show

data LogEntry = LogEntry_BeginsShift Minute GuardId
              | LogEntry_FallsAsleep Minute
              | LogEntry_WakesUp Minute
              deriving (Show)

justAsleep :: GuardStates -> [Minute]
justAsleep = fmap snd . filter ((==Asleep) . fst) . unGuardStates

mostCommonMinute :: GuardStates -> (Int, Minute)
mostCommonMinute =
  head . reverse . sort . fmap (\ms -> (length ms, head ms)) . group . sort . justAsleep

guardSolve :: (GuardId, GuardStates) -> Int
guardSolve (guardId, states) = unGuardId guardId * unMinute (snd $ mostCommonMinute states)

worstGuard :: (Ord a) => (GuardStates -> a) -> Map GuardId GuardStates -> (GuardId, GuardStates)
worstGuard f = snd . maximum . fmap (\(k, v) -> (f v, (k, v))) . assocs

processLog :: LogEntry -> Maybe State -> Maybe State
processLog (LogEntry_BeginsShift m gId) _ = Just $ State gId Awake m
processLog l Nothing = error $ "impossible input data with no state: " <> show l
processLog (LogEntry_FallsAsleep m) (Just (State gId _ _)) = Just $ State gId Asleep m
processLog (LogEntry_WakesUp m) (Just (State gId _ _)) = Just $ State gId Awake m

-- couldn't find this in Data.Map?
upsert :: (Monoid v, Ord k) => k -> v -> Map k v -> Map k v
upsert k v m = case lookup k m of
  Nothing -> insert k v m
  Just existing -> insert k (v <> existing) m

interpretStates :: [State] -> Map GuardId GuardStates
interpretStates states = foldl' f mempty $ zip states (tail states)
  where f stateMap (State gId1 s1 m1, State gId2 _ m2) = case gId1 == gId2 of
          False -> stateMap
          True -> upsert gId1 (GuardState $ zip (repeat s1) [ m1 .. (m2 - 1) ]) stateMap

processLogs :: [LogEntry] -> [State]
processLogs = reverse . catMaybes . foldl' f []
  where f [] logEntry = [processLog logEntry Nothing]
        f (currentState:states) logEntry = (processLog logEntry currentState):currentState:states

makeLog :: String -> LogEntry
makeLog raw = case drop 15 raw of
  (m1:m2:_:_:"wakes up") -> LogEntry_WakesUp (Minute . read $ (m1:m2:[]))
  (m1:m2:_:_:"falls asleep") -> LogEntry_FallsAsleep (Minute . read $ (m1:m2:[]))
  (m1:m2:_:_:switch) -> LogEntry_BeginsShift (Minute . read $ (m1:m2:[])) $
    (GuardId . read . takeWhile (/=' ') . drop 7 $ switch)
  _ -> error "bad input"

main = do
  guardData <-
    interpretStates . processLogs . fmap makeLog . sort . lines <$> readFile "./data/dayFourData.txt"
  print . guardSolve . worstGuard (length . justAsleep) $ guardData
  print . guardSolve . worstGuard mostCommonMinute $ guardData
