{-# LANGUAGE OverloadedStrings #-}

import Elm.Render.Render ( writeElmPages )
import Fitness.Running
import Fitness.Garmin

toy :: IO ()
toy = do
  activities <- getActivitiesFromDir fitFileDir
  let runningMap = mapWithDay $ filterBySport [Run, TrailRun] activities
      week20 = getByYearAndWeek 2020 19 runningMap
      f0 :: (Int, [Activity]) -> (Int, [RunningMetrics])
      f0 (day, activities) = (day, mkRunningMetrics <$> activities)
      rms = map (map mkRunningMetrics . snd) week20
  mapM_ print (f0 <$> week20)


main :: IO ()
main = do
  writeElmPages
