{-# LANGUAGE OverloadedStrings #-}

import Fitness.Running
import Fitness.Garmin

main :: IO ()
main = do
  rms <- getRunningMetrics
  pure ()
