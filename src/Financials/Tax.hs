-- | Only two things in life are certain.
module Financials.Tax
  ( payTaxes,
  )
where

import Financials.Investments (Income (..), PostTax, PreTax)
import Financials.Utils

-- TODO (matte): Add joint accounts, PROGRESSIVE TAX SYSTEM
payTaxes :: Income PreTax -> Income PostTax
payTaxes (Income income) = (Income income) - medicareTax - socialSecurity - caTax - federalTax
  where
    medicareTax :: Income PostTax
    medicareTax = Income $ getPercentage (Percentage 1.45) income
    socialSecurity :: Income PostTax
    socialSecurity = Income $ getPercentage (Percentage 6.2) income
    -- California state income tax.
    caTax :: Income PostTax
    caTax = Income $ getPercentage taxRate income
      where
        taxRate
          | income < 8544 = Percentage 1
          | income < 20256 = Percentage 2
          | income < 31970 = Percentage 4
          | income < 44378 = Percentage 6
          | income < 56086 = Percentage 8
          | income < 286493 = Percentage 9.3
          | income < 343789 = Percentage 10.3
          | income < 572981 = Percentage 11.3
          | otherwise = Percentage 12.3
    -- Rate	For Unmarried Individuals.
    federalTax :: Income PostTax
    federalTax = Income $ getPercentage taxRate income
      where
        taxRate
          | income < 9700 = Percentage 0
          | income < 39475 = Percentage 12
          | income < 84200 = Percentage 22
          | income < 160725 = Percentage 24
          | income < 204100 = Percentage 32
          | income < 510300 = Percentage 35
          | otherwise = Percentage 35
