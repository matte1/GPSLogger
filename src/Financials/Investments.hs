{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | TODO
module Financials.Investments
  ( Accounts (..),
    -- TODO remove this export!
    ContributionLimit (..),
    Income (..),
    InvestmentAccount (..),
    Personal,
    PostTax,
    PreTax,
    Roth,
    Traditional,
    contribute,
    compound,
    invest,
    limitTrad401K,
    limitRoth401K,
    resetAllContributions,
  )
where

import Financials.Utils

-- | Investment Account Types
data Roth

data Traditional

data Personal

-- | Tax Types
data PreTax

data PostTax

data Accounts
  = Accounts
      { tradinal401k :: InvestmentAccount PreTax Traditional,
        rothIRA :: InvestmentAccount PostTax Roth,
        personal :: InvestmentAccount PostTax Personal,
        savings :: Income PostTax
      }
  deriving (Show)

resetAllContributions :: Accounts -> Accounts
resetAllContributions a0 =
  Accounts
    { tradinal401k = resetContributions (tradinal401k a0),
      rothIRA = resetContributions (rothIRA a0),
      personal = resetContributions (personal a0),
      savings = savings a0
    }

data InvestmentAccount tax account
  = InvestmentAccount
      { principal :: Double,
        earnings :: Double,
        contributionsYTD :: Double
      }
  deriving (Show)

resetContributions :: InvestmentAccount tax account -> InvestmentAccount tax account
resetContributions a0 = a0 {contributionsYTD = 0}

compound :: InvestmentAccount tax account -> Percentage -> InvestmentAccount tax account
compound account rate = account {earnings = newEarnings}
  where
    newEarnings = earnings account + getPercentage rate (principal account + earnings account)

newtype Income tax = Income {unIncome :: Double}
  deriving (Show, Eq, Ord, Num)

newtype ContributionLimit account = ContributionLimit {getContributionLimit :: Double}
  deriving (Eq, Ord, Num)

-- TODO Consolidate these two constants
limitRoth401K :: ContributionLimit Roth
limitRoth401K = 19000

-- TODO Consolidate these two constants
limitTrad401K :: ContributionLimit Traditional
limitTrad401K = 19000

-- limitRothIra :: ContributionLimit Traditional
-- limitRothIra = 6000

invest :: InvestmentAccount PostTax Personal -> Income PostTax -> InvestmentAccount PostTax Personal
invest a i = snd $ contribute (ContributionLimit 1e9) (Percentage 100) Nothing a i

contribute ::
  ContributionLimit account ->
  Percentage ->
  Maybe Percentage ->
  InvestmentAccount tax account ->
  Income tax ->
  (Income tax, InvestmentAccount tax account)
contribute (ContributionLimit limit) rate mmatching account (Income income) =
  updateAccount (min maxContribution desiredContribution)
  where
    maxContribution = limit - contributionsYTD account
    desiredContribution = getPercentage rate income
    updateAccount :: Double -> (Income tax, InvestmentAccount tax account)
    updateAccount amount =
      ( Income (income - amount),
        account
          { principal = principal account + amount + employerContribution mmatching,
            contributionsYTD = contributionsYTD account + amount
          }
      )
      where
        employerContribution (Just r) = getPercentage r amount
        employerContribution Nothing = 0
