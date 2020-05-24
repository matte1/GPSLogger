{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Elm.Render.Render
  ( writeElmPages,
  )
where

import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Elm.Render.Fitness (renderFitnessPage)
import Elm.Render.Running (renderRunningPage)
import Fitness.Garmin

writeElmPages :: IO ()
writeElmPages = do
  currentTime <- getCurrentTime
  activities <- getActivitiesFromDir fitFileDir
  runningPage <- renderRunningPage currentTime activities
  fitnessPage <- renderFitnessPage currentTime activities
  TIO.writeFile "/home/matt/projects/LifeOfMatt/src/Elm/Fitness/Autogen/Fitness.elm" fitnessPage
  TIO.writeFile "/home/matt/projects/LifeOfMatt/src/Elm/Fitness/Autogen/Running.elm" runningPage
-- TIO.putStr fitnessPage
