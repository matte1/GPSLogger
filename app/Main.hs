{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Elm.Render.Render (writeElmPages)
import Fitness.Garmin
import Fitness.Running
import Financials.Example

getActivities :: IO [Activity]
getActivities = getActivitiesFromDir fitFileDir

weeklyRunningStats :: [Activity] -> Int -> IO ()
weeklyRunningStats activities week = do
  let runningMap = mapWithDay $ filterBySport [Run, TrailRun] activities
      week20 =
        mconcat
          $ map (mconcat . (map mkRunningMetrics . snd))
          $ getByYearAndWeek 2020 week runningMap
  print week20

yearlyRunningStats :: [Activity] -> IO ()
yearlyRunningStats activities = do
  let runningMap = mapWithYear $ filterBySport [Run, TrailRun] activities
      rmsByYear = M.map (map mkRunningMetrics) runningMap
      rmByYear = M.map mconcat rmsByYear
      rmByYear' = M.toList rmByYear
  mapM_ (\(year, rm) -> print "------------------" >> print year >> print rm) rmByYear'

main :: IO ()
main = do
  writeElmPages
  pure ()
