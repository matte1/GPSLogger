POST_TAX_INCOME = 60 * 1000
INFLATION_RATE = 0.00

LOAN1 = \
  { "rate": 0.04,
    "principle": 40 * 1000,
    "name": "Ang"
  }

LOAN2 = \
  { "rate": 0.08,
    "principle": 80 * 1000,
    "name": "Parentals"
  }

class Portfolio():
  def __init__(self):
    self.principle = 0
    self.earnings = 0

  def run_month(self, investment, annual_growth_rate):
    '''Invest money and compound current value.'''
    self.earnings += (self.principle + self.earnings) * (annual_growth_rate / 12.)
    self.principle += investment

  def __str__(self):
    return f'''
Investment Portfolio:
Total: {self.principle + self.earnings}
Principle: {self.principle}
Earnings: {self.earnings}'''


class Loan():
  def __init__(self, annual_rate, principle, name):
    self.annual_rate = annual_rate
    self.principle = principle
    self.interest_paid = 0
    self.name = name
    self._paid_in_full = principle == 0

  def paid_in_full(self):
    '''Returns true if the loan has been paid in full.'''
    return self._paid_in_full

  def monthly_minimum(self, payment):
    '''Pays the minimum amount on the loan and returns the remainder.'''
    interest_due = self.principle * self.annual_rate / 12.
    assert payment > interest_due, "You must pay the interest at minimum!"
    # Accrue the interest paid for comparing cost of loan
    self.interest_paid += interest_due
    return payment - interest_due

  def run_month(self, payment):
    '''Calculate monthly payment.'''
    actual_payment = min(self.principle, payment)
    self.principle -= actual_payment
    return payment - actual_payment


  def __str__(self):
    '''Prints a helpful summary of the loan.'''
    return f'''
Name: {self.name}
Total Interest Paid: {self.interest_paid}
Principle: {self.principle}'''

# Splits money between 2 loans and an investment account. Enforces that the minimum on the
# loans is paid each month.
def run_strategy(savings_rate, market_rate, splits):
  # Amount of money available for paying off loans and investing after taxes and living expenses.
  monthly_mula = (POST_TAX_INCOME / 12.) * savings_rate

  assert sum([val for _, val in splits.items()]) <= 1.0, 'Splits must sum to 1!'

  portfolio = Portfolio()
  loan1 = Loan(LOAN1['rate'], LOAN1['principle'], LOAN1['name'])
  loan2 = Loan(LOAN2['rate'], LOAN2['principle'], LOAN2['name'])

  for year in range(10):
    for month in range(12):
      # Pay the bare minimum for the first and second loan.
      mula = loan1.monthly_minimum(monthly_mula)
      mula = loan2.monthly_minimum(mula)
      # Split money between the first loan and stock portfolio
      loan1_mula = splits['loan1'] * mula
      loan2_mula = splits['loan2'] * mula
      portfolio_mula = (1 - splits['loan2'] - splits['loan1']) * mula
      # Pay loans
      loan1_leftover = loan1.run_month(loan1_mula)
      loan2_leftover = loan2.run_month(loan2_mula)
      # All leftovers go into portfolio
      portfolio.run_month(portfolio_mula + loan1_leftover + loan2_leftover, market_rate)
    monthly_mula += monthly_mula * INFLATION_RATE

  print("==============================================")
  print(portfolio)
  print(loan1)
  print(loan2)
  net_worth = portfolio.principle + portfolio.earnings - loan1.principle - loan2.principle
  print()
  print(f'NetWorth: {net_worth}')
  return net_worth

# run_strategy(0.20, 0.07, {'loan1': 0.1, 'loan2': 0.9})
#
# run_strategy(0.50, 0.07, {'loan1': 0.1, 'loan2': 0.9})
# run_strategy(0.50, 0.07, {'loan1': 0.5, 'loan2': 0.5})
run_strategy(0.50, 0.07, {'loan1': 0.1, 'loan2': 0.9})
# run_strategy(0.50, 0.07, {'loan1': 0.0, 'loan2': 0.0})

# run_strategy(0.75, 0.07, {'loan1': 0.5, 'loan2': 0.5})
# run_strategy(0.85, 0.07, {'loan1': 0.5, 'loan2': 0.5})
