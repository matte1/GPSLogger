{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Module for working with jsons derived from fit file.
module Fitness.Garmin
    ( Activity(..)
    , Record(..)
    , Sport(..)
    , sortActivitiesByWeekAndSport
    , extractActivityIfJson
    , getActivitiesFromDir
    , fitFileDir
    ) where

import Fitness.Utils (appendItemToListInMap, pairs)

import Data.Aeson hiding (pairs)
import Data.Time.Calendar.WeekDate
import qualified Data.Map.Lazy as M
import GHC.Generics ( Generic )
import Data.Time.Clock ( UTCTime(..), NominalDiffTime(..), diffUTCTime)
import System.Directory (listDirectory)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import PyF ( fmt )

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
  deriving (Eq, Generic, Show)
instance FromJSON Sport

data Activity =
  Activity
  { sport :: Sport
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

fitFileDir :: FilePath
fitFileDir = "/home/matt/projects/LifeOfMatt/data/garmin/jsons/"

extractActivityIfJson :: FilePath -> IO (Maybe Activity)
extractActivityIfJson file =
  if T.isSuffixOf ".json" $ T.pack file
    then do
      bytes <- B.readFile file
      case eitherDecode bytes of
        Left err -> error [fmt|\nFailed to parse {file}\n{err}\n|]
        Right json -> pure (Just json)
    else
      pure Nothing

getActivitiesFromDir :: FilePath -> IO [Activity]
getActivitiesFromDir path = do
  files <- listDirectory path
  mactivities <- mapM extractActivityIfJson $ (path <>) <$> files
  pure (catMaybes mactivities)

sortActivitiesByWeekAndSport :: [Sport] -> [Activity] -> M.Map (Int, Int) [Activity]
sortActivitiesByWeekAndSport sports as = foldl insertByWeek M.empty as
  where
    insertByWeek :: M.Map (Int, Int) [Activity] -> Activity -> M.Map (Int, Int) [Activity]
    insertByWeek map' a@(Activity sport records) =
      if sport `elem` sports
        then appendItemToListInMap (fromIntegral year, week) a map'
        else map'
      where
        (year, week, _day) = toWeekDate . utctDay . timestamp . head $ records
