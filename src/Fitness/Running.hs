{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Algorithms for measuring running performance.
-- TODO
-- 1) plot runs with pace +/- epsilon against heart rate.
--   * If pace is consistently X +/- (X * 0.05)
--   * If heartrate is consistently Y +/- (Y * 0.05)
-- 2) Generate training plans algoithmically..

module Fitness.Running
    ( exportRunningStats
    , printNominalDiffTimeAsHours
    ) where

import Fitness.Garmin
import Fitness.HeartRate

import Data.Time.Clock ( NominalDiffTime(..) )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import PyF ( fmt )

data WeeklyRunning
  = WeeklyRunning
    { miles :: Double
    , elevation :: Double
    , heartRateZones :: HeartRateZones NominalDiffTime
    } deriving Show

getWeeklyRunnings :: M.Map (Int, Int) [Activity] -> M.Map (Int, Int) WeeklyRunning
getWeeklyRunnings runsByWeek = M.map sumMiles runsByWeek
  where
    f0 :: Maybe Double -> Double
    f0 (Just distance) = distance
    f0 Nothing = error "Unacceptable nothing!"

    f1 :: [Record] -> Double
    f1 records = f0 . distance $ last records


    sumMiles :: [Activity] -> WeeklyRunning
    sumMiles activities =
      WeeklyRunning
      { miles =
          let distancesPerWeek = map (\activity -> f1 (records activity)) activities
          in (sum distancesPerWeek)  / 1600
      , elevation = 0
      , heartRateZones = foldl1 (\hz1 hz2 -> (+) <$> hz1 <*> hz2) $ fmap getTimeInHrZones activities
      }


----------------------------------------------------------------------------------------------------
-- Rendering
-- TODO: Make a separate file for this
----------------------------------------------------------------------------------------------------
-- HACK: I can't figure out how to get the double value out of NominalDiffTime so I'm reading
-- it as a String...
printNominalDiffTimeAsHours :: NominalDiffTime -> Double
printNominalDiffTimeAsHours seconds =
  read (T.unpack $ T.replace "s" "" (T.pack . show $ seconds / 3600))

csvWriter :: (Int, Int) -> WeeklyRunning -> T.Text
csvWriter (year, week) wr =
  let hrz = (pure printNominalDiffTimeAsHours) <*> (heartRateZones wr)
      totalTime = sum hrz
  in [fmt|\
{week},\
{miles wr:.2},\
{elevation wr},\
{totalTime:.2},\
{zone1 hrz:.2},\
{zone2 hrz:.2},\
{zone3 hrz:.2},\
{zone4 hrz:.2},\
{zone5 hrz:.2}\
|]

writeWeeklyRunningToLine ::
  ((Int, Int) -> WeeklyRunning -> T.Text) ->
  M.Map (Int, Int) WeeklyRunning ->
  T.Text
writeWeeklyRunningToLine writer weeks =
  T.intercalate "\n" .
  map snd . M.toList $
  M.mapWithKey writer weeks

-- milesByWeek = TODO
-- elevByWeek = TODO
-- paceByWeek = TODO
exportRunningStats :: FilePath -> IO ()
exportRunningStats output = do
  activities <- getActivitiesFromDir fitFileDir
  let runsByWeek = sortActivitiesByWeekAndSport [Run, TrailRun] activities
      csv = writeWeeklyRunningToLine csvWriter (getWeeklyRunnings runsByWeek)
      csvWithHeader :: T.Text
      csvWithHeader = [fmt|week,miles,elevation,zone1,zone2,zone3,zone4,zone5|]
  TIO.writeFile output [fmt|{csvWithHeader}\n{csv}\n|]
