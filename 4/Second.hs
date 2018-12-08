import qualified Data.Map.Strict as M
import Data.Map ((!))
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.List (findIndex, groupBy, sortOn, sort, maximumBy)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

--- IO
    
readInput :: String -> IO [String]
readInput filename = lines <$> readFile filename
    
--- parsing

parseDate :: String -> Maybe UTCTime 
parseDate = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M"

data Event = WakesUp | FallsAsleep | BeginsShift Int deriving Show 
data TimedEvent = TimedEvent {
    utcTime :: UTCTime,
    event :: Event
} deriving Show 

parseEvent :: String -> Maybe Event
parseEvent s = case s of
    "wakes up" -> Just WakesUp
    "falls asleep" -> Just FallsAsleep
    otherwise -> 
        let s' = drop 7 s
            endIndex = findIndex ((==) ' ') s'
            s'' = flip take s' <$> endIndex
            guardId = s'' >>= readMaybe
        in BeginsShift <$> guardId

parseTimedEvent :: String -> Maybe TimedEvent
parseTimedEvent s = 
    let parsedEvent = parseEvent . drop 19 $ s
        parsedDate = parseDate . drop 1 . take 17 $ s
    in TimedEvent <$> parsedDate <*> parsedEvent

readTimedEvents :: [String] -> [TimedEvent]
readTimedEvents = sortOn utcTime . mapMaybe parseTimedEvent

--- calculating sleep periods

data SleepPeriod = SleepPeriod {
    start :: UTCTime,
    end :: UTCTime
} deriving Show

data CalcState = Start | Awake Int UTCTime | Asleep Int UTCTime deriving Show

calcSleepPeriods :: [TimedEvent] -> [(Int, SleepPeriod)]
calcSleepPeriods timedEvents =
    let go :: CalcState -> [TimedEvent] -> [(Int, SleepPeriod)] -> [(Int, SleepPeriod)]
        go _ [] acc = acc
        go state (e:es) acc = let eventTime = utcTime e in case (event e, state) of
            (FallsAsleep, Awake guardId end) -> go (Asleep guardId eventTime) es acc
            (WakesUp, Asleep guardId start) -> go (Awake guardId eventTime) es ((guardId, SleepPeriod start eventTime) : acc)
            (BeginsShift guardId, _) -> go (Awake guardId eventTime) es acc
            _ -> error $ "unhandled transition " ++ (show state)
    in go Start timedEvents []

groupAssocs :: Ord k => [(k, a)] -> M.Map k [a]
groupAssocs assocs = M.fromListWith (++) [(k, [v]) | (k, v) <- assocs]    

sleepPeriods :: [TimedEvent] -> M.Map Int [SleepPeriod]
sleepPeriods = groupAssocs . calcSleepPeriods

--- finding longest sleeper

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

sleepSeconds :: SleepPeriod -> Int
sleepSeconds period = floor $ diffUTCTime (end period) (start period)

findLongestSleeper :: M.Map Int [SleepPeriod] -> Int
findLongestSleeper guardSleepPeriods = 
    let guardSleepTimes = (fmap sum) $ (fmap sleepSeconds) <$> guardSleepPeriods
        guardSleepTimesList = M.toList guardSleepTimes
    in fst $ maxBy snd guardSleepTimesList

minutesList :: Int -> Int -> [Int]
minutesList start num =
    let minutes = [start..59] ++ cycle [0..59]
    in take num minutes

sleepMinutes :: SleepPeriod -> [Int]
sleepMinutes (SleepPeriod start end) = 
    let durationInMins = floor $ diffUTCTime end start / 60 
        startMins = todMin . localTimeOfDay . utcToLocalTime utc $ start
    in minutesList startMins durationInMins

numberOfTimesOnFavouriteSleepMinute :: [SleepPeriod] -> (Int, Int)
numberOfTimesOnFavouriteSleepMinute sleepPeriods = 
    let maxSleepMins = maxBy length $ groupBy (==) $ sort $ concatMap sleepMinutes sleepPeriods
    in (head &&& length) maxSleepMins

main :: IO ()
main = do
    input <- readInput "input.txt"
    let timedEvents = readTimedEvents input
    let guardSleepPeriods = sleepPeriods timedEvents
    let maxSleepMinutes = M.toList $ numberOfTimesOnFavouriteSleepMinute <$> guardSleepPeriods
    let (g, (min, count)) = maxBy (\(_, (_, count)) -> count) maxSleepMinutes
    print $ g * min
    