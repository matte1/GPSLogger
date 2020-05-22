{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Elm.Render.Render
    ( writeElmPages
    ) where

import Elm.Render.Running (renderRunningPage)
import Elm.Render.Fitness (renderFitnessPage)

import Fitness.Garmin
import Data.Time.Clock ( UTCTime(..), getCurrentTime )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PyF ( fmt )

writeElmPages :: IO ()
writeElmPages = do
  currentTime <- getCurrentTime
  activities <- getActivitiesFromDir fitFileDir

  runningPage <- renderRunningPage currentTime activities
  fitnessPage <- renderFitnessPage currentTime activities

  TIO.writeFile "/home/matt/projects/LifeOfMatt/src/Elm/Fitness/Autogen/Fitness.elm" fitnessPage
  TIO.writeFile "/home/matt/projects/LifeOfMatt/src/Elm/Fitness/Autogen/Running.elm" runningPage
  
  -- TIO.putStr fitnessPage
