{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Elm.Render.Render
    ( writeElmPages
    ) where

import Elm.Render.Running (renderRunningPage)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PyF ( fmt )

writeElmPages :: IO ()
writeElmPages = do
  runningPage <- renderRunningPage
  TIO.writeFile "/home/matt/projects/LifeOfMatt/src/Elm/Running/Page.elm" runningPage
