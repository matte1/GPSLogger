{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | TODO
module Financials.Utils
    ( Percentage(..)
    , getPercentage
    , convertAnnualReturnToMonthly
    ) where

newtype Percentage = Percentage Double
  deriving Show

getPercentage :: Percentage -> Double -> Double
getPercentage (Percentage p) n = n * p / 100

convertAnnualReturnToMonthly :: Percentage -> Percentage
convertAnnualReturnToMonthly (Percentage annual) = Percentage (annual ^ 1/12)
