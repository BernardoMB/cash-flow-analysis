# Discounted cash flows

source("discounted-cash-flows.R")

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate

# ---- Landings ----

valueLandings <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Project
  projectType="01Landings", # Project type
  averagePrice=10000, # Expected price of the project
  sdPrice=2000, # Standar deviation of the price of the project
  lambda=12*2, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(1, 2, 3),
  probs=c(4/6, 1/6, 1/6),
  # Payment scheme
  advanceFeeRate=1, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.025 # Percentaje of the price of the project to be charged monthly for project maintenance
)

# ---- Small projects ----

valueSmallProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Project
  projectType="02SmallProjects", # Project type
  averagePrice=50000, # Expected price of the project
  sdPrice=20000, # Standar deviation of the price of the project
  lambda=12, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   2,   3,    4,    5,   6,   7,   8),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

# ---- Large projects ----

valueLargeProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  projectType="03LargeProjects", # Project type
  averagePrice=200000, # Expected price of the project
  sdPrice=70000, # Standar deviation of the price of the project
  lambda=8, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   5,   6,    7,    8,   9,   10,   11),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

# ---- Enterprice projects ----

valueEnterpriceProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  projectType="04EnterpriceProjects", # Project type
  averagePrice=1600000, # Expected price of the project
  sdPrice=200000, # Standar deviation of the price of the project
  lambda=2, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   13,   14,    15,    16,   17,   18,   19),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

# ---- Inhouse projects ----

valueInhouseProjects <- getInhouseProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Project
  projectType="05InhouseProjects", # Project type
  lambda=1.5, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   10,   11,    12,    13,   14,   15,   16),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  averageMonthlyRent=200000, # Project average monthly rent
  sdMonthlyRent=30 # Project monthly rent standar deviation
)

# ---- Total value ----

totalValue <- valueLandings$`Projects PV` + 
  valueSmallProjects$`Projects PV` + 
  valueLargeProjects$`Projects PV` + 
  valueEnterpriceProjects$`Projects PV` + 
  valueInhouseProjects$`Projects PV`

# ---- Costs ----

building <- 60000
intenet <- 1300
water <- 40 * 2 * 4
parking <- 2000 * 5
salaries <- 70000 * 7 + 40000
monthlyCost <- building + intenet + water + parking + salaries
initialCost <- 200000
r <- 0.05 # Anual inflation rate
t <- 1 # Years
y <- 0.10 # Annual interest rate
y12 <- (1 + y) ^ (1 / 12) - 1 # Monthly effective interest rate
costPresentValue <- 0
annuitySum <- 0
months2 <- 12 * t
for (k in 1:months2) {
  annuitySum <- annuitySum + 1 / (1 + y12) ^ k
}
monthlyCostPresentValue <- monthlyCost*annuitySum
totalCost <- initialCost + monthlyCostPresentValue