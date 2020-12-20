from pulp import LpMaximize, LpProblem, LpStatus, lpSum, LpVariable

LOAN1 = 30*1000
MONTHLY_INTEREST1 = 0.04 / 12.0

LOAN2 = 80*1000
MONTHLY_INTEREST2 = 0.08 / 12.0

ANNUAL_SALARY = 109 * 1000
TAX_RATE = 0.30
SAVINGS_RATE = 0.50

MONTHLY_CONTRIBUTIONS = (SAVINGS_RATE * ANNUAL_SALARY * (1 - TAX_RATE)) / 12.0
MONTHLY_COMPOUNDING = 0.07 / 12.0

print(MONTHLY_CONTRIBUTIONS)

NUM_MONTHS = 10 * 12

# Define the model
model = LpProblem(name="loan_optimization", sense=LpMaximize)

# Define the decision variables
loan1 = \
  {i: LpVariable(name=f"l1_{i}", lowBound=0) for i in range(NUM_MONTHS)}
loan_contribution1 = \
  {i: LpVariable(name=f"lc1_{i}", lowBound=0) for i in range(NUM_MONTHS)}
interest1 = \
  {i: LpVariable(name=f"interest1_{i}", lowBound=0) for i in range(NUM_MONTHS)}

loan2 = \
  {i: LpVariable(name=f"l2_{i}", lowBound=0) for i in range(NUM_MONTHS)}
loan_contribution2 = \
  {i: LpVariable(name=f"lc2_{i}", lowBound=0) for i in range(NUM_MONTHS)}
interest2 = \
  {i: LpVariable(name=f"interest2_{i}", lowBound=0) for i in range(NUM_MONTHS)}

portfolio = \
  {i: LpVariable(name=f"portfolio{i}", lowBound=0) for i in range(NUM_MONTHS)}
portfolio_contribution = \
  {i: LpVariable(name=f"portfolio_contribution{i}", lowBound=0) for i in range(NUM_MONTHS)}

model += (portfolio[0] == 0, 'portfolio_c0')
model += (loan1[0] == LOAN1 - loan_contribution1[0], 'loan1_c0')
model += (loan2[0] == LOAN2 - loan_contribution2[0], 'loan2_c0')
model += (interest1[0] == LOAN1 * MONTHLY_INTEREST1, f'interest1_c0')
model += (interest2[0] == LOAN2 * MONTHLY_INTEREST2, f'interest2_c0')
model += \
  ( portfolio_contribution[0] + \
    interest1[0] + \
    interest2[0] + \
    loan_contribution1[0] + \
    loan_contribution2[0] == MONTHLY_CONTRIBUTIONS
  , f'contributions_c{0}'
  )

# Add constraints
for idx in range(1, NUM_MONTHS):
  # Contribute to portfolio and compound
  model += \
    ( portfolio[idx-1] + \
      portfolio_contribution[idx] + \
      portfolio[idx-1] * MONTHLY_COMPOUNDING == portfolio[idx]
    , f'portfolio_c{idx}'
    )
  # Pay off loan
  model += (loan1[idx] == loan1[idx-1] - loan_contribution1[idx], f'loan1_c{idx}')
  model += (loan2[idx] == loan2[idx-1] - loan_contribution2[idx], f'loan2_c{idx}')
  # Pay interest on loan
  model += (interest1[idx] == loan1[idx-1] * MONTHLY_INTEREST1, f'interest1_c{idx}')
  model += (interest2[idx] == loan2[idx-1] * MONTHLY_INTEREST2, f'interest2_c{idx}')

  # Constrain contributions to always equal the monthly contributions
  model += \
    ( portfolio_contribution[idx] + \
      interest1[idx] + \
      interest2[idx] + \
      loan_contribution1[idx] + \
      loan_contribution2[idx] == MONTHLY_CONTRIBUTIONS
    , f'contributions_c{idx}'
    )


# This regularization term is here to slightly prioritize having no debt without
# changing the numerical result by much.
reg0 = 1
reg1 = 1
reg2 = 1

# Set the objective function
model += \
    reg0 * portfolio[NUM_MONTHS-1]  \
  - reg1 * loan1[NUM_MONTHS-1] \
  - reg2 * loan2[NUM_MONTHS-1]

# Solve the optimization problem
status = model.solve()

# Get the results
print(f"status: {model.status}, {LpStatus[model.status]}")
print(f"objective: {model.objective.value()}")
print('---------------')

def fmt(var):
  return f'{var.name}\t{round(var.value()):6}'

for idx in range(len(interest1)):
  # print(f'{fmt(interest1[idx])} | {fmt(interest2[idx])} | {fmt(loan_contribution1[idx])} | {fmt(loan_contribution2[idx])} | {fmt(loan1[idx])} | {fmt(loan2[idx])} | {fmt(portfolio[idx])}')
  print(f'{fmt(loan1[idx])} | {fmt(loan2[idx])} | {fmt(portfolio[idx])}')
