{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- | Module for working with jsons derived from fit file.
module Fitness.Garmin
    ( Activity(..)
    , Record(..)
    , Sport(..)

     -- * Data Extraction
    , unsafeTotalDistance
    , altitudeOrZero
    , distanceOrZero
    , heartRateOrZero
    , changes
    , integrate
    , filterBySport
    , getByYearAndWeek
    , mapWithDay

      -- * File IO
    , readActivity
    , getActivitiesFromDir
    , fitFileDir
    ) where

import Fitness.Utils

import Data.Aeson hiding (pairs)
import Data.Maybe ( fromMaybe )
import Data.Time.Calendar ( Day )
import Data.Time.Calendar.WeekDate
import qualified Data.Map.Lazy as M
import GHC.Generics ( Generic )
import Data.Time.Clock ( UTCTime(..), NominalDiffTime(..), diffUTCTime)
import System.Directory (listDirectory)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import PyF ( fmt )
import Data.Time.LocalTime


data Sport
  = Run
  | TrailRun
  | SUP
  | Climb
  | BikeIndoor
  | MTB
  | Hike
  | Abs
  | Bike
  | Climbing
  | Stopwatch
  | Yoga
  | Navigate
  | Bouldering
  | Strength
  | RollOut
  | ClimbingWall -- TODO(matte): Remove!
  deriving (Eq, Generic, Show)
instance FromJSON Sport

data Activity =
  Activity
  { filename :: String
  , sport :: Sport
  , records :: [Record]
  } deriving (Show, Generic)
instance FromJSON Activity

data Record =
  Record
  { altitude           :: Maybe Double
  , cadence            :: Maybe Double
  , distance           :: Maybe Double
  , fractionalCadence  :: Maybe Double
  , heartRate          :: Maybe Double
  , lat                :: Maybe Double
  , long               :: Maybe Double
  , speed              :: Maybe Double
  , temperature        :: Maybe Double
  , timestamp          :: UTCTime
  } deriving (Show, Read, Generic)
instance FromJSON Record

unsafeTotalDistance :: Activity -> Double
unsafeTotalDistance activity
  | total == 0 = error "unsafeTotalDistance: You've encountered a malformed activity!"
  | otherwise = total
  where
    total = distanceOrZero . last $ records activity

-- | Extract the distance from a record if its 'Just' else return a 0.
distanceOrZero :: Record -> Double
distanceOrZero Record{distance=Just d} = d
distanceOrZero Record{distance=Nothing} = 0

-- | Extract the heart rate from a record if its 'Just' else return a 0.
heartRateOrZero :: Record -> Double
heartRateOrZero Record{heartRate=Just d} = d
heartRateOrZero Record{heartRate=Nothing} = 0

-- | Extract the altitude from a record if its 'Just' else return a 0.
altitudeOrZero :: Record -> Double
altitudeOrZero Record{altitude=Just d} = d
altitudeOrZero Record{altitude=Nothing} = 0

integrate :: [Record] -> (Record -> Double) -> Double
integrate records getter =
  foldl (\accum (r, dt) -> accum + getter r * dt) 0 (zip records dts)
  where
    dts :: [Double]
    dts = dt . (\(r1, r2) -> (timestamp r1, timestamp r2)) <$> pairs records

changes :: [Record] -> (Record -> Double) -> [Double]
changes records getter = (\(r1, r2) -> getter r2 - getter r1) <$> pairs records

fitFileDir :: FilePath
fitFileDir = "/home/matt/projects/LifeOfMatt/data/garmin/jsons/"

readActivity :: FilePath -> IO Activity
readActivity file =
  if T.isSuffixOf ".json" $ T.pack file
    then do
      bytes <- B.readFile file
      case eitherDecode bytes of
        Left err -> error [fmt|\nFailed to parse {file}\n{err}\n|]
        Right json -> pure json
    else
      error [fmt|What the fuck is this {file} doing here?|]

getActivitiesFromDir :: FilePath -> IO [Activity]
getActivitiesFromDir path = do
  files <- listDirectory path
  mapM readActivity $ (path <>) <$> files

filterBySport :: [Sport] -> [Activity] -> [Activity]
filterBySport sports = filter (\activity -> sport activity `elem` sports)

getByYearAndWeek :: Integer -> Int -> M.Map Day [a] -> [(Int, [a])]
getByYearAndWeek year week map' =
  [ ( day
    , fromMaybe [] (map' M.!? fromWeekDate year week day)
    )
  | day <- [1..7]
  ]

mapWithDay :: [Activity] -> M.Map Day [Activity]
mapWithDay activities = foldl insertBy M.empty activities
  where
    insertBy :: M.Map Day [Activity] -> Activity -> M.Map Day [Activity]
    insertBy m0 activity =
      appendItemToListInMap
      (getPstDay $ timestamp . last $ records activity)
      m0
      activity
