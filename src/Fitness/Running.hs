{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Algorithms for measuring running performance.
-- TODO
-- 1) plot runs with pace +/- epsilon against heart rate.
--   * If pace is consistently X +/- (X * 0.05)
--   * If heartrate is consistently Y +/- (Y * 0.05)
-- 2) Generate training plans algoithmically..
module Fitness.Running
  ( RunningMetrics (..),
    mkRunningMetrics,
  )
where

import Fitness.Garmin
import Fitness.HeartRate
import Fitness.Utils
import PyF (fmt)

data RunningMetrics
  = RunningMetrics
      { meters :: Double,
        metersPerHeartBeat :: Double,
        timeInHrZones :: HeartRateZones Double,
        elevationGain :: Double,
        elevationLoss :: Double,
        totalHeartBeats :: Double,
        totalTime :: Double,
        rmFilename :: String
      }

instance Show RunningMetrics where
  show rm =
    [fmt|\
miles: {(meters rm / 1600):.2}
metersPerHeartBeat: {(metersPerHeartBeat rm * 100):.2}
timeInHrZones: {show $ toHoursMins <$> timeInHrZones rm}
elevationGain: {(elevationGain rm):.2}
totalTime: {toHoursMins (totalTime rm)}
|]

instance Semigroup RunningMetrics where
  (<>) rm1 rm2 =
    RunningMetrics
      { meters = meters rm1 + meters rm2,
        metersPerHeartBeat = (meters rm1 + meters rm2) / (totalHeartBeats rm1 + totalHeartBeats rm2),
        timeInHrZones = (+) <$> timeInHrZones rm1 <*> timeInHrZones rm2,
        elevationGain = elevationGain rm1 + elevationGain rm2,
        elevationLoss = elevationLoss rm1 + elevationLoss rm2,
        totalHeartBeats = totalHeartBeats rm1 + totalHeartBeats rm2,
        totalTime = totalTime rm1 + totalTime rm2,
        rmFilename = rmFilename rm1 <> " : " <> rmFilename rm2
      }
instance Monoid RunningMetrics where
  mempty = RunningMetrics 0 0 (pure 0) 0 0 0 0 ""

mkRunningMetrics :: Activity -> RunningMetrics
mkRunningMetrics activity = running
  where
    running =
      RunningMetrics
        { meters = unsafeTotalDistance activity,
          metersPerHeartBeat = meters running / totalHeartBeats running,
          timeInHrZones = getTimeInHrZones activity,
          elevationGain = sum $ (max 0) <$> elevationChanges,
          elevationLoss = sum $ (min 0) <$> elevationChanges,
          totalHeartBeats = integrate rs heartRateOrZero,
          totalTime = dt (timestamp $ head rs, timestamp $ last rs),
          rmFilename = filename activity
        }
    elevationChanges = changes rs altitudeOrZero
    rs = records activity

-- getHeartBeatsOfActivites :: [Activity] -> Double
-- getHeartBeatsOfActivites = sum . fmap (flip integrateRecords getHeartRateOrZero) . fmap records
--
-- getDistanceOfActivities :: [Activity] -> Double
-- getDistanceOfActivities = sum . fmap totalDistance
--
-- getWeeklyRunnings :: M.Map (Int, Int) [Activity] -> M.Map (Int, Int) WeeklyRunning
-- getWeeklyRunnings runsByWeek = M.map sumMiles runsByWeek
--   where
--     f0 :: Maybe Double -> Double
--     f0 (Just distance) = distance
--     f0 Nothing = error "Unacceptable nothing!"
--
--     f1 :: [Record] -> Double
--     f1 records = f0 . distance $ last records
--
--     sumMiles :: [Activity] -> WeeklyRunning
--     sumMiles activities =
--       WeeklyRunning
--       { miles =
--           let distancesPerWeek = map (\activity -> f1 (records activity)) activities
--           in (sum distancesPerWeek)  / 1600
--       , elevation = 0
--       , heartRateZones = foldl1 (\hz1 hz2 -> (+) <$> hz1 <*> hz2) $ fmap getTimeInHrZones activities
--       , averageHrPower = getDistanceOfActivities activities / getHeartBeatsOfActivites activities
--       }
--
----------------------------------------------------------------------------------------------------
-- Rendering
-- TODO: Make a separate file for this
----------------------------------------------------------------------------------------------------
--
-- csvWriter :: (Int, Int) -> WeeklyRunning -> T.Text
-- csvWriter (year, week) wr =
--   let hrz = (pure (\x -> getDoubleFromNominalDiffTime x / 3600)) <*> (heartRateZones wr)
--       totalTime = sum hrz
--   in [fmt|\
--     {week},\
--     {miles wr:.2},\
--     {(100*averageHrPower wr):.2},\

-- | ]
--  -- {totalTime:.2},\
--  -- {zone1 hrz:.2},\
--  -- {zone2 hrz:.2},\
--  -- {zone3 hrz:.2},\
--  -- {zone4 hrz:.2},\
--  -- {zone5 hrz:.2}\
--
--  writeWeeklyRunningToLine ::
--    ((Int, Int) -> WeeklyRunning -> T.Text) ->
--    M.Map (Int, Int) WeeklyRunning ->
--    T.Text
--  writeWeeklyRunningToLine writer weeks =
--    T.intercalate "\n" .
--    map snd . M.toList $
--    M.mapWithKey writer weeks
--
--  exportRunningStats :: FilePath -> IO ()
--  exportRunningStats output = do
--    activities <- getActivitiesFromDir fitFileDir
--    let runsByWeek = filterByWeekAndSport [Run, TrailRun] activities
--        csv = writeWeeklyRunningToLine csvWriter (getWeeklyRunnings runsByWeek)
--        csvWithHeader :: T.Text
--        csvWithHeader = [fmt|week,miles,elevation,zone1,zone2,zone3,zone4,zone5|]
--    TIO.writeFile output [fmt|{csvWithHeader}\n{csv}\n|]
