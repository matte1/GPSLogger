{-# LANGUAGE OverloadedStrings #-}

import Running

main :: IO ()
main = do
  exportRunningStats "/home/matt/projects/LifeOfMatt/data/garmin/running_stats.csv"
