{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Algorithms for measuring running performance.
module Fitness.Running
    ( exportRunningStats
    , printNominalDiffTimeAsHours
    ) where

import Fitness.Garmin
import Fitness.HeartRate

import Data.Time.Clock ( NominalDiffTime(..) )
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

-- plot runs with pace +/- epsilon against heart rate.
--  * If pace is consistently X +/- (X * 0.05)
--  * If heartrate is consistently Y +/- (Y * 0.05)
-- calculateAeT :: [Record] -> Bool
-- calculateAeT =

-- Generate training plans algoithmically..

-- HACK: I can't figure out how to get the double value out of NominalDiffTime so I'm reading
-- it as a String...
printNominalDiffTimeAsHours :: NominalDiffTime -> Double
printNominalDiffTimeAsHours seconds =
  read (T.unpack $ T.replace "s" "" (T.pack . show $ seconds / 3600))

writeWeeklyRunningsToCsv :: M.Map (Int, Int) WeeklyRunning -> T.Text
writeWeeklyRunningsToCsv =
    T.intercalate "\n" .
    map snd . M.toList .
    M.mapWithKey weeklyRunningToCsv
  where
    weeklyRunningToCsv :: (Int, Int) -> WeeklyRunning -> T.Text
    weeklyRunningToCsv (year, week) wr =
      let hrz :: HeartRateZones Double
          hrz = (pure printNominalDiffTimeAsHours) <*> (heartRateZones wr)
          totalTime = sum hrz
      in [fmt|\
{date},\
{week},\
{miles wr},\
{elevation wr},\
{totalTime:.2},\
{zone1 hrz:.2},\
{zone2 hrz:.2},\
{zone3 hrz:.2},\
{zone4 hrz:.2},\
{zone5 hrz:.2}\
|]

-- milesByWeek = TODO
-- elevByWeek = TODO
-- paceByWeek = TODO
exportRunningStats :: FilePath -> IO ()
exportRunningStats output = do
  activities <- getActivitiesFromDir fitFileDir
  let runsByWeek = sortActivitiesByWeekAndSport [Run, TrailRun] activities
      hrZoneByWeek = getTimeInHrZonesPerWeek runsByWeek
      weeklyRunningByWeek = M.map (\hrZone -> WeeklyRunning 0 0 hrZone) hrZoneByWeek
      csv = writeWeeklyRunningsToCsv weeklyRunningByWeek
      csvWithHeader :: T.Text
      csvWithHeader = [fmt|week,miles,elevation,zone1,zone2,zone3,zone4,zone5|]
  TIO.writeFile output [fmt|{csvWithHeader}\n{csv}\n|]
