{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Financials.Example (example) where

import Financials.Investments
import Financials.Tax
import Financials.Utils

myAccounts :: Accounts
myAccounts =
  Accounts
    { tradinal401k =
        InvestmentAccount
          { principal = 90000,
            earnings = 0,
            contributionsYTD = 28500
          },
      rothIRA =
        InvestmentAccount
          { principal = 58000,
            earnings = 0,
            contributionsYTD = 0
          },
      personal =
        InvestmentAccount
          { principal = 106000,
            earnings = 0,
            contributionsYTD = 0
          },
      savings = Income 0
    }

max401ContributionRate :: Percentage
max401ContributionRate = Percentage 50

averageMonthlyGrowth :: Percentage
averageMonthlyGrowth = convertAnnualReturnToMonthly (Percentage 6)

runMonth :: Accounts -> Income PreTax -> Accounts
runMonth accounts income =
  compoundedAccounts
    { tradinal401k = newTraditional401k,
      personal = newPersonal,
      savings = savings compoundedAccounts + remainingIncomePostTax
    }
  where
    compoundedAccounts =
      Accounts
        { tradinal401k = compound (tradinal401k accounts) averageMonthlyGrowth,
          rothIRA = compound (rothIRA accounts) averageMonthlyGrowth,
          personal = compound (personal accounts) averageMonthlyGrowth,
          savings = savings accounts
        }
    (remainingIncomePreTax, newTraditional401k) =
      contribute
        limitTrad401K
        max401ContributionRate
        (Just $ Percentage 50)
        (tradinal401k compoundedAccounts)
        income
    postTaxIncome = payTaxes remainingIncomePreTax
    (remainingIncomePostTax, newPersonal) =
      contribute
        (ContributionLimit 1e9)
        (Percentage 70)
        Nothing
        (personal compoundedAccounts)
        postTaxIncome

example :: IO ()
example = do
  let myIncomes = replicate 12 (Income 12000)
      year1 = foldl runMonth myAccounts myIncomes
      year2 = foldl runMonth (resetAllContributions year1) myIncomes
      year3 = foldl runMonth (resetAllContributions year2) myIncomes
      year4 = foldl runMonth (resetAllContributions year3) myIncomes
      year5 = foldl runMonth (resetAllContributions year4) myIncomes
      year6 = foldl runMonth (resetAllContributions year5) myIncomes
      year7 = foldl runMonth (resetAllContributions year6) myIncomes
      year8 = foldl runMonth (resetAllContributions year7) myIncomes
      year9 = foldl runMonth (resetAllContributions year8) myIncomes
      year10 = foldl runMonth (resetAllContributions year9) myIncomes
      year11 = foldl runMonth (resetAllContributions year10) myIncomes
      year12 = foldl runMonth (resetAllContributions year11) myIncomes
      year13 = foldl runMonth (resetAllContributions year12) myIncomes
      year14 = foldl runMonth (resetAllContributions year13) myIncomes
      year15 = foldl runMonth (resetAllContributions year14) myIncomes
      year16 = foldl runMonth (resetAllContributions year15) myIncomes
      year17 = foldl runMonth (resetAllContributions year16) myIncomes
      year18 = foldl runMonth (resetAllContributions year17) myIncomes
      year19 = foldl runMonth (resetAllContributions year18) myIncomes
      year20 = foldl runMonth (resetAllContributions year19) myIncomes
  print "Year 1"
  print year1
  print "Year 5"
  print year5
  print "Year 7"
  print year7
  print "Year 10"
  print year10
  -- print "Year 14"
  -- print year14
  -- print "Year 16"
  -- print year16
  -- print "Year 20"
  -- print year20
