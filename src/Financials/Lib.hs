{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Financials.Lib
  (
  )
where

--   Income(..)
-- , Percentage(..)
-- , PreTax, PostTax
-- , payTaxes

-- data Portfolio =
--   Portfolio
--   { pTraditional401K :: Double
--   , pTraditionalIRA :: Double
--   , pRoth401K :: Double
--   , pRothIRA :: Double
--   , pBrokerageAccount :: Double
--   , pIRAContribution :: Double
--   , p401KContribution :: Double
--   }

-- data Compensation =
--   Compensation
--   { cMonthlySalary :: Double
--   , c401Contribution :: Percentage
--   , c401Matching :: Percentage
--   }

-- stepMonth :: Double -> Investment401K -> Investment401K
-- stepMonth income i401k = traditional401k
--   where
--     (remainingIncome, traditional401k) = contributeTo401K i401k (Percentage 50) income

-- oneMonth :: Portfolio -> Compensation -> Portfolio
-- oneMonth portfolio compensation = updatedPortfolio
--   where
--     preTaxContributions = ?
--     postTaxContributions = ?

-- estimateHoneyPot :: Age -> StartingIncome -> HoneyPot -> TaxBrackets -> ExpectedGrowth -> HoneyPot
