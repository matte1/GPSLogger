{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Elm.Render.Render (writeElmPages)
import Fitness.Garmin
import Fitness.Running

activities :: IO [Activity]
activities = getActivitiesFromDir fitFileDir

weeklyRunningStats :: Int -> IO ()
weeklyRunningStats week = do
  activities <- getActivitiesFromDir fitFileDir
  let runningMap = mapWithDay $ filterBySport [Run, TrailRun] activities
      week20 =
        concatRunningMetrics
          $ map (concatRunningMetrics . (map mkRunningMetrics . snd))
          $ getByYearAndWeek 2020 week runningMap
  print week20

yearlyRunningStats :: IO ()
yearlyRunningStats = do
  activities <- getActivitiesFromDir fitFileDir
  let runningMap = mapWithYear $ filterBySport [Run, TrailRun] activities
      rmsByYear = M.map (map mkRunningMetrics) runningMap
      rmByYear = M.map concatRunningMetrics rmsByYear
      rmByYear' = M.toList rmByYear
  mapM_ (\(year, rm) -> print "------------------" >> print year >> print rm) rmByYear'

main :: IO ()
main = do
  writeElmPages
  pure ()
