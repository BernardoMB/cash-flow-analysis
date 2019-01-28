# Discounted cash flows

# Irving Marin
# Bernardo Mondragon
# Eduardo Picazo
# Sebastian Gonzalez
# Jose Aznar
# Enrique Scherer
# Farid Hajnal
# Jose Mijares

library(ggplot2)
library(lubridate)

# ---- Utility functions ----

sampleFromMonthsDistribution <- function(sop, probs) {
  if (sum(probs) != 1) {
    stop(paste("Probs should add up 1. Got ", sum(probs)))
  }
  probsAc <- cumsum(probs)
  unif <- runif(1)
  for (i in 1:length(probsAc)) {
    if (unif < probsAc[i]) {
      return(sop[i])
    }  
  }
}

sumMonths <- function(date, months) {
  result <- date + lubridate:::months.numeric(months)
  if (is.na(result)) {
    result <- date + lubridate:::days(3) + lubridate:::months.numeric(months)
    return(result)
  }
  return(result)
}

# ---- Functions ----

getProjectsCashFlows <- function(
  t, # Lifetime of the company
  # Mexican economy
  y, # Annual interest rate
  r, # Inflation rate
  # Project
  projectType, # Project type
  averagePrice, # Expected price of the project
  sdPrice, # Standar deviation of the price of the project
  lambda, # Expected number of projects developed in a year
  # Project development time distribution
  sop,
  probs,
  # Payment scheme
  advanceFeeRate, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate, # Percentage of the final price that is paid during the development of the project
  finalFeeRate, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate # Percentaje of the price of the project to be charged monthly for project maintenance
) {
  # Calculate monthly effective interest rate
  y12 <- ( 1 + y ) ^ ( 1 / 12) - 1
  time <- 0
  arrivalTime <- 0
  positiveFlows <- data.frame()
  now <- as_date(Sys.time())
  valuationPeriodEndDate <- now + years(t)
  presentValues <- c()
  while (time < t) {
    interarrivalTime <- rexp(n = 1, rate = lambda)
    time <- time + interarrivalTime 
    if (time < t) {
      arrivalTime <- arrivalTime + interarrivalTime
      price <- rlnorm(1, mean = log(averagePrice), sd = 1 / log(sdPrice))
      adjustedPrice <- price * (1 + r) ^ arrivalTime
      advanceFee <- adjustedPrice * advanceFeeRate
      arrivalTimeInSeconds <- as.numeric(duration(arrivalTime, unit = "years"))
      arrivalDate <- as_date(now + seconds(arrivalTimeInSeconds))
      entry <- list(arrivalDate, advanceFee, TRUE, "Advance fee")
      positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
      developmentMonths <- sampleFromMonthsDistribution(sop = sop, probs = probs)
      monthlyFee <- adjustedPrice * monthlyFeeRate / developmentMonths
      endDate <- sumMonths(arrivalDate, developmentMonths)
      if (endDate < valuationPeriodEndDate) {
        # Case 1
        for (i in 1:developmentMonths) {
          monthlyFeeDate <- sumMonths(arrivalDate, i)
          
          entry <- list(monthlyFeeDate, monthlyFee, TRUE, "Monthly fee")
          positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
        }
        finalFee <- adjustedPrice * finalFeeRate
        entry <- list(endDate, finalFee, TRUE, "Final fee")
        positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
        count <- 1
        monthlyRent <- adjustedPrice * monthlyRentRate
        repeat {
          monthlyRentDate <- sumMonths(endDate, count)
          if (monthlyRentDate > valuationPeriodEndDate) {
            break
          }
          entry <- list(monthlyRentDate, monthlyRent, TRUE, "Monthly rent")
          positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
          count <- count + 1
        }
        rentMonths <- count - 1
        discountFactor <- 1 / (1 + y) ^ arrivalTime
        advanceFeePresentValue <- discountFactor * advanceFee
        fractionalAnnuity <- (1 - (1 / (1 + y12)) ^ developmentMonths) / y12
        monthlyFeesPresentValue <- discountFactor * monthlyFee * fractionalAnnuity
        developmentMonthsDiscountFactor <- 1 / (1 + y) ^ (developmentMonths / 12)
        finalFeePresentValue <- discountFactor * developmentMonthsDiscountFactor * finalFee
        fractionalAnnuityRents <- (1 - (1 / (1 + y12)) ^ rentMonths) / y12
        monthlyRentsPresentValue <- discountFactor * developmentMonthsDiscountFactor * monthlyRent * fractionalAnnuityRents
        presentValue <- advanceFeePresentValue + monthlyFeesPresentValue + finalFeePresentValue + monthlyRentsPresentValue
        presentValues <- c(presentValues, presentValue)
      } else {
        # Case 2
        count <- 1
        repeat {
          monthlyFeeDate <- sumMonths(arrivalDate, count)
          if (monthlyFeeDate > valuationPeriodEndDate) {
            break
          }
          entry <- list(monthlyFeeDate, monthlyFee, TRUE, "Monthly fee")
          positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
          count <- count + 1
        }
        discountFactor <- 1 / (1 + y) ^ arrivalTime
        advanceFeePresentValue <- discountFactor * advanceFee
        feeMonths <- count - 1
        fractionalAnnuity <- (1 - (1 / (1 + y12)) ^ feeMonths) / y12
        monthlyFeesPresentValue <- discountFactor * monthlyFee * fractionalAnnuity
        presentValue <- advanceFeePresentValue + monthlyFeesPresentValue
        presentValues <- c(presentValues, presentValue)
      }
    }
  }
  names(positiveFlows) <- c("Date","Amount","Income","Concept")
  breaksVec <- c(seq(from=floor_date(as_date(Sys.time()), unit="month"),
                     to=ceiling_date(max(as_date(positiveFlows$Date)), unit="month"),
                     by="6 months"))
  coloresChidos <- c("hotpink", "gold", "darkorange", "coral4")
  revenuesPlot <- ggplot(data=positiveFlows, aes(x=as_date(positiveFlows$Date), y=positiveFlows$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(positiveFlows$Amount)+1000)) +
    xlim(c(as_date(Sys.time()),max(as_date(positiveFlows$Date)))) +
    labs(title="Revenues", x="Time (years)", y="Income") +
    geom_linerange(aes(x=as_date(positiveFlows$Date), ymax=positiveFlows$Amount, ymin=0, color=positiveFlows$Concept)) +
    guides(color=guide_legend(title="Types of income")) +
    scale_x_date(breaks = breaksVec, date_minor_breaks = "1 month", limits = c(as_date(Sys.time()), NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #geom_linerange(aes(x=as_datetime(positiveFlows$Date), ymax=positiveFlows$Amount, ymin=0), color="#00AFBB") +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Advance fee"),]$Amount), size=1, color=coloresChidos[1]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Monthly fee"),]$Amount), size=1, color=coloresChidos[2]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Final fee"),]$Amount), size=1, color=coloresChidos[3]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Monthly rent"),]$Amount), size=1, color=coloresChidos[4]) +
  #scale_color_manual(values=coloresChidos)
  arrivalsDf <- positiveFlows[positiveFlows$Concept %in% c("Advance fee"),]
  arrivalsPlot <- ggplot(data=arrivalsDf, aes(x=as_date(arrivalsDf$Date), y=c(0))) + 
    ylim(c(0,0)) +
    labs(title="Project arrivals", x="Time", y="") +
    theme(aspect.ratio=0.1, 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_hline(yintercept=0, size=0.5, color="cyan") +
    geom_point(shape=4, size=2) 
  projectsPresentValue <- sum(presentValues)
  # Get anual revenues
  annualRevenues <- data.frame()
  for (i in 1:t) {
    lowerBound <- now + lubridate:::years(i - 1)
    upperBound <- lowerBound + lubridate:::years(1)
    df <- positiveFlows[lowerBound <= as_date(positiveFlows$Date) & as_date(positiveFlows$Date) < upperBound,]
    total.advance.payments <- sum(df[df$Concept %in% c("Advance fee"),]$Amount)
    total.monthly.payments <- sum(df[df$Concept %in% c("Monthly fee"),]$Amount)
    total.final.payments <- sum(df[df$Concept %in% c("Final fee"),]$Amount)
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    period <- lowerBound %--% upperBound
    entry <- list(as.factor(period), total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
    annualRevenues <- rbind(annualRevenues, entry, stringsAsFactors = FALSE)
  }
  names(annualRevenues) <- c("Period","Advance fees","Monthly fees","Final fees","Monthly rents")
  write.csv(annualRevenues, file=paste(projectType,"RevenueData.csv",sep=""))
  result <- list(
    positiveFlows,
    projectsPresentValue,
    annualRevenues,
    revenuesPlot,
    arrivalsPlot
  )
  names(result) <- c("Flows", "Projects PV", "Annual revenues", "Revenues", "Arrivals")
  return(result)
}

getInhouseProjectsCashFlows <- function(
  t, # Lifetime of the company
  # Mexican economy
  y, # Annual interest rate
  r, # Inflation rate
  # Project
  projectType, # Project type
  lambda, # Expected number of projects developed in a year
  # Development time distribution
  sop,
  probs,
  # Payment scheme
  averageMonthlyRent, # Project average monthly rent
  sdMonthlyRent # Project monthly rent standar deviation
) {
  # Calculate monthly effective interest rate
  y12 <- ( 1 + y ) ^ ( 1 / 12) - 1
  
  time <- 0
  arrivalTime <- 0
  positiveFlows <- data.frame()
  now <- as_date(Sys.time())
  valuationPeriodEndDate <- now + years(t)
  arrivals <- c()
  presentValues <- c()
  while (time < t) {
    interarrivalTime <- rexp(n = 1, rate = lambda)
    time <- time + interarrivalTime 
    if (time < t) {
      arrivalTime <- arrivalTime + interarrivalTime
      arrivalTimeInSeconds <- as.numeric(duration(arrivalTime, unit = "years"))
      arrivalDate <- as_date(now + seconds(arrivalTimeInSeconds))
      arrivals <- c(arrivals, arrivalDate)
      developmentMonths <- sampleFromMonthsDistribution(sop = sop, probs = probs)
      endDate <- sumMonths(arrivalDate, developmentMonths)
      if (endDate < valuationPeriodEndDate) {
        # Case 1
        count <- 1
        repeat {
          monthlyRentDate <- sumMonths(endDate, count)
          if (monthlyRentDate > valuationPeriodEndDate) {
            break
          }
          monthlyRent <- rlnorm(1, mean = log(averageMonthlyRent), sd = 1 / log(sdMonthlyRent))
          entry <- list(monthlyRentDate, monthlyRent, TRUE, "Monthly rent")
          positiveFlows <- rbind(positiveFlows, entry, stringsAsFactors = FALSE)
          discountFactor <- 1 / (1 + y) ^ (arrivalTime + (developmentMonths / 12) + (count / 12))
          presentValue <- discountFactor * monthlyRent 
          presentValues <- c(presentValues, presentValue)
          count <- count + 1
        }
      }
    }
  }
  names(positiveFlows) <- c("Date","Amount","Income","Concept")
  breaksVec <- c(seq(from=floor_date(as_date(Sys.time()), unit="month"),
                     to=ceiling_date(max(as_date(positiveFlows$Date)), unit="month"),
                     by="6 months"))
  coloresChidos <- c("hotpink", "gold", "darkorange", "coral4")
  revenuesPlot <- ggplot(data=positiveFlows, aes(x=as_date(positiveFlows$Date), y=positiveFlows$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(positiveFlows$Amount)+1000)) +
    xlim(c(as_date(Sys.time()),max(as_date(positiveFlows$Date)))) +
    labs(title="Revenues", x="Time (years)", y="Income") +
    geom_linerange(aes(x=as_date(positiveFlows$Date), ymax=positiveFlows$Amount, ymin=0, color=positiveFlows$Concept)) +
    guides(color=guide_legend(title="Types of income")) +
    scale_x_date(breaks = breaksVec, date_minor_breaks = "1 month", limits = c(as_date(Sys.time()), NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #geom_linerange(aes(x=as_datetime(positiveFlows$Date), ymax=positiveFlows$Amount, ymin=0), color="#00AFBB") +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Advance fee"),]$Amount), size=1, color=coloresChidos[1]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Monthly fee"),]$Amount), size=1, color=coloresChidos[2]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Final fee"),]$Amount), size=1, color=coloresChidos[3]) +
  #geom_hline(yintercept=mean(positiveFlows[positiveFlows$Concept %in% c("Monthly rent"),]$Amount), size=1, color=coloresChidos[4]) +
  #scale_color_manual(values=coloresChidos)
  arrivalsPlot <- ggplot(data=data.frame(arrivals), aes(x=as_date(arrivals), y=c(0))) + 
    ylim(c(0,0)) +
    labs(title="Project arrivals", x="Time", y="") +
    theme(aspect.ratio=0.1, 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_hline(yintercept=0, size=0.5, color="cyan") +
    geom_point(shape=4, size=2) 
  projectsPresentValue <- sum(presentValues)
  # Get anual revenues
  annualRevenues <- data.frame()
  for (i in 1:t) {
    lowerBound <- now + lubridate:::years(i - 1)
    upperBound <- lowerBound + lubridate:::years(1)
    df <- positiveFlows[lowerBound <= as_date(positiveFlows$Date) & as_date(positiveFlows$Date) < upperBound,]
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    period <- lowerBound %--% upperBound
    entry <- list(as.factor(period), total.rent.payments)
    annualRevenues <- rbind(annualRevenues, entry, stringsAsFactors = FALSE)
  }
  names(annualRevenues) <- c("Period", "Monthly rents")
  write.csv(t(annualRevenues), file=paste(projectType,"RevenueData.csv",sep=""))
  result <- list(
    positiveFlows,
    projectsPresentValue,
    annualRevenues,
    revenuesPlot,
    arrivalsPlot
  )
  names(result) <- c("Flows", "Projects PV", "Annual revenues", "Revenues", "Arrivals")
  return(result)
}

# ---- Simulation ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate

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

valueSmallProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Project
  projectType="02SmallProjects", # Project type
  averagePrice=50000, # Expected price of the project
  sdPrice=20000, # Standar deviation of the price of the project
  lambda=5, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   2,   3,    4,    5,   6,   7,   8),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

valueLargeProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  projectType="03LargeProjects", # Project type
  averagePrice=200000, # Expected price of the project
  sdPrice=70000, # Standar deviation of the price of the project
  lambda=2, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   10,   11,    12,    13,   14,   15,   16),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

valueEnterpriceProjects <- getProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  projectType="04EnterpriceProjects", # Project type
  averagePrice=1600000, # Expected price of the project
  sdPrice=200000, # Standar deviation of the price of the project
  lambda=1, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   13,   14,    15,    16,   17,   18,   19),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  advanceFeeRate=0.25, # Percentage of the final price that is paid at the begining of the project
  monthlyFeeRate=0.5, # Percentage of the final price that is paid during the development of the project
  finalFeeRate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

valueInhouseProjects <- getInhouseProjectsCashFlows(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Project
  projectType="05InhouseProjects", # Project type
  lambda=1, # Expected number of projects developed in a year
  # Development time distribution
  sop=c(   10,   11,    12,    13,   14,   15,   16),
  probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37),
  # Payment scheme
  averageMonthlyRent=400000, # Project average monthly rent
  sdMonthlyRent=100000 # Project monthly rent standar deviation
)

totalValue <- valueLandings$`Projects PV` + 
  valueSmallProjects$`Projects PV` + 
  valueLargeProjects$`Projects PV` + 
  valueEnterpriceProjects$`Projects PV` + 
  valueInhouseProjects$`Projects PV`

# Costs
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