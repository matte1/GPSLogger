{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fitness.HeartRate
  ( HeartRateZones (..),
    getTimeInHrZonesPerWeek,
    getTimeInHrZones,
  )
where

import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Calendar.WeekDate
import Data.Time.Clock (NominalDiffTime (..), UTCTime (..), diffUTCTime)
import Fitness.Garmin
import Fitness.Utils
import GHC.Generics (Generic)
import System.Directory (listDirectory)

data HeartRateZones a
  = HeartRateZones
      { zone1 :: a,
        zone2 :: a,
        zone3 :: a,
        zone4 :: a,
        zone5 :: a
      }
  deriving (Show)

instance Functor HeartRateZones where
  fmap g hz =
    HeartRateZones
      { zone1 = g (zone1 hz),
        zone2 = g (zone2 hz),
        zone3 = g (zone3 hz),
        zone4 = g (zone4 hz),
        zone5 = g (zone5 hz)
      }

instance Foldable HeartRateZones where
  foldr f x0 hrz =
    zone5 hrz `f` (zone4 hrz `f` (zone3 hrz `f` (zone2 hrz `f` (zone1 hrz `f` x0))))

instance Applicative HeartRateZones where
  pure a = HeartRateZones a a a a a
  (<*>) g hz =
    HeartRateZones
      { zone1 = (zone1 g) (zone1 hz),
        zone2 = (zone2 g) (zone2 hz),
        zone3 = (zone3 g) (zone3 hz),
        zone4 = (zone4 g) (zone4 hz),
        zone5 = (zone5 g) (zone5 hz)
      }

-- | This is my rough guesstimate at my heart rate zones.
heartRateZones :: HeartRateZones Double
heartRateZones =
  HeartRateZones
    { zone1 = 114,
      zone2 = 133,
      zone3 = 152,
      zone4 = 171,
      zone5 = 184
    }

-- | Accumulate a value by the heart rate zone at which it was done in.
accumByHeartRateZone :: Num a => Double -> a -> HeartRateZones a -> HeartRateZones a
accumByHeartRateZone hr value zones = (+) <$> valueInZone <*> zones
  where
    z0 = pure 0
    valueInZone
      | hr < zone2 heartRateZones = z0 {zone1 = value}
      | hr < zone3 heartRateZones = z0 {zone2 = value}
      | hr < zone4 heartRateZones = z0 {zone3 = value}
      | hr < zone5 heartRateZones = z0 {zone4 = value}
      | otherwise = z0 {zone5 = value}

-- | Accumulate the amount of time in seconds spent in each hear rate zone per week.
getTimeInHrZonesPerWeek ::
  M.Map (Int, Int) [Activity] ->
  M.Map (Int, Int) (HeartRateZones Double)
getTimeInHrZonesPerWeek activitiesByWeek =
  M.map (foldl1 (\hz1 hz2 -> (+) <$> hz1 <*> hz2) . fmap getTimeInHrZones) activitiesByWeek

-- | Get the time spent in a heart rate zone per Activity.
getTimeInHrZones :: Activity -> HeartRateZones Double
getTimeInHrZones = foldl accumTimeInHrZone (pure 0) . pairs . records
  where
    accumTimeInHrZone ::
      HeartRateZones Double ->
      (Record, Record) ->
      HeartRateZones Double
    accumTimeInHrZone timeInZones (r1, r2) = accumByHeartRateZone meanHr dt timeInZones
      where
        meanHr = calcMeanHr (heartRate r1) (heartRate r2)
        calcMeanHr :: Maybe Double -> Maybe Double -> Double
        calcMeanHr (Just hr1) (Just hr2) = (hr1 + hr2) / 2
        calcMeanHr (Just hr1) Nothing = hr1
        calcMeanHr Nothing (Just hr2) = hr2
        calcMeanHr Nothing Nothing = 0
        dt :: Double
        dt = doubleFromNominalDiffTime $ diffUTCTime (timestamp r2) (timestamp r1)
