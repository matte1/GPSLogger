{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

data Sport = Run | TrailRun | SUP | Climb
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
  { altitude           :: Double
  , cadence            :: Double
  , distance           :: Double
  , fractionalCadence  :: Double
  , heartRate          :: Double
  , lat                :: Double
  , long               :: Double
  , speed              :: Double
  , temperature        :: Double
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
      pure (decode bytes)
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
