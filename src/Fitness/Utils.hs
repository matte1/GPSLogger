{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Fitness.Utils
  ( appendItemToListInMap,
    average,
    pairs,

    -- * Time
    doubleFromNominalDiffTime,
    dt,
    toHoursMins,
    getPstDay,
  )
where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (NominalDiffTime (..), UTCTime (..), diffUTCTime)
import Data.Time.LocalTime
import PyF (fmt)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

appendItemToListInMap :: Ord k => k -> M.Map k [a] -> a -> M.Map k [a]
appendItemToListInMap key map' value =
  case (map' M.!? key) of
    Just vs -> M.insert key (value : vs) map'
    Nothing -> M.insert key [value] map'

-- HACK: I can't figure out how to get the double value out of NominalDiffTime so I'm reading
-- it as a String...
doubleFromNominalDiffTime :: NominalDiffTime -> Double
doubleFromNominalDiffTime = read . T.unpack . T.replace "s" "" . T.pack . show

average :: (Fractional a, Num a) => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

dt :: (UTCTime, UTCTime) -> Double
dt (t1, t2) = doubleFromNominalDiffTime $ diffUTCTime t2 t1

toHoursMins :: Double -> T.Text
toHoursMins seconds =
  let (hours, mins) = properFraction $ seconds / 3600 :: (Int, Double)
      mins' = round (mins * 60) :: Int
   in [fmt|{hours}:{mins':.2}|]

getPstDay :: UTCTime -> Day
getPstDay utc =
  let pstTimeZone :: TimeZone
      pstTimeZone = TimeZone (-8 * 60) False "PST"
   in localDay $ utcToLocalTime pstTimeZone utc
