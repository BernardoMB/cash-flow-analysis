# Functions for simulating future cash flows and calculate their present value

library(ggplot2)
library(lubridate)

source("./utils.R")

# ---- Projects ----

#' Get the present value of each future cash flow that the company will have
#' during the projection period due to the development of the specified type
#' of project acording to the provided pricing scheme.
#'
#' @param t Lifetime of the company
#' Mexican economy:
#' @param y Annual interest rate
#' @param r Inflation rate
#' Project:
#' @param project_type Project type
#' @param averagePrice Expected price negociated with the client
#' @param sdPrice Standar deviation of the negociated price with the client
#' @param lambda Expected number of projects developed in a year
#' Projected development time distribution: 
#' @param sop Support of the discrete distribution of the development time measured in months
#' @param probs Probabilities of the discrete destribution of the development time measured in months 
#' Payment scheme (Rethink this assumtion. Is it a good assumption? Is it accurate?):
#' @param advanceFeeRate Percentage of the final price that is paid at the begining of the project
#' @param monthlyFeeRate Percentage of the final price that is paid during the development of the project
#' @param finalFeeRate Percentage of the final price that is paid at the end of the development of the project
#' @param monthlyRentRate Percentaje of the price of the project to be charged monthly for project maintenance
#' @return A list that contains the simulated future revenues, the present value of those revenues, simulated monthly revenues, simulated annual revenues, revenue time series and the arrivals of projects in time.
#' @examples
#' getProjectCashFlows(3, 0.07, -0.01, "LargeProject", 100000, 30000, 3, c(1, 2, 3), c(4/6, 1/6, 1/6), 0.5, 0, 0.5, 0.025)
getProjectsCashFlows <- function(t, y, r, projectType, averagePrice, sdPrice, lambda, sop, probs, advanceFeeRate, monthlyFeeRate, finalFeeRate, monthlyRentRate) {
  # Calculate monthly effective interest rate
  y12 <- ( 1 + y ) ^ ( 1 / 12) - 1
  time <- 0
  arrivalTime <- 0
  revenues <- data.frame()
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
      revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
      developmentMonths <- sampleFromMonthsDistribution(sop = sop, probs = probs)
      monthlyFee <- adjustedPrice * monthlyFeeRate / developmentMonths
      endDate <- sumMonths(arrivalDate, developmentMonths)
      if (endDate < valuationPeriodEndDate) {
        # Case 1: project development finishes before valuation period ends
        for (i in 1:developmentMonths) {
          monthlyFeeDate <- sumMonths(arrivalDate, i)
          entry <- list(monthlyFeeDate, monthlyFee, TRUE, "Monthly fee")
          revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
        }
        finalFee <- adjustedPrice * finalFeeRate
        entry <- list(endDate, finalFee, TRUE, "Final fee")
        revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
        count <- 1
        monthlyRent <- adjustedPrice * monthlyRentRate
        repeat {
          monthlyRentDate <- sumMonths(endDate, count)
          if (monthlyRentDate > valuationPeriodEndDate) {
            break
          }
          entry <- list(monthlyRentDate, monthlyRent, TRUE, "Monthly rent")
          revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
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
        # Case 2: project development finishes after valuation period ends
        count <- 1
        repeat {
          monthlyFeeDate <- sumMonths(arrivalDate, count)
          if (monthlyFeeDate > valuationPeriodEndDate) {
            break
          }
          entry <- list(monthlyFeeDate, monthlyFee, TRUE, "Monthly fee")
          revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
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
  names(revenues) <- c("Date","Amount","Money MXN","Concept")
  breaksVec <- c(seq(from=floor_date(as_date(Sys.time()), unit="month"),
                     to=ceiling_date(max(as_date(revenues$Date)), unit="month"),
                     by="6 months"))
  coloresChidos <- c("hotpink", "gold", "darkorange", "coral4")
  revenuesPlot <- ggplot(data=revenues, aes(x=as_date(revenues$Date), y=revenues$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(revenues$Amount)+1000)) +
    xlim(c(as_date(Sys.time()),max(as_date(revenues$Date)))) +
    labs(title="Revenues", x="Time", y="Money MXN") +
    geom_linerange(aes(x=as_date(revenues$Date), ymax=revenues$Amount, ymin=0, color=revenues$Concept)) +
    guides(color=guide_legend(title="Types of Money MXN")) +
    scale_x_date(breaks = breaksVec, date_minor_breaks = "1 month", limits = c(as_date(Sys.time()), NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #geom_linerange(aes(x=as_datetime(revenues$Date), ymax=revenues$Amount, ymin=0), color="#00AFBB") +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Advance fee"),]$Amount), size=1, color=coloresChidos[1]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Monthly fee"),]$Amount), size=1, color=coloresChidos[2]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Final fee"),]$Amount), size=1, color=coloresChidos[3]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Monthly rent"),]$Amount), size=1, color=coloresChidos[4]) +
  #scale_color_manual(values=coloresChidos)
  arrivalsDf <- revenues[revenues$Concept %in% c("Advance fee"),]
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
    df <- revenues[lowerBound <= as_date(revenues$Date) & as_date(revenues$Date) < upperBound,]
    total.advance.payments <- sum(df[df$Concept %in% c("Advance fee"),]$Amount)
    total.monthly.payments <- sum(df[df$Concept %in% c("Monthly fee"),]$Amount)
    total.final.payments <- sum(df[df$Concept %in% c("Final fee"),]$Amount)
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    #period <- lowerBound %--% upperBound
    entry <- list(i, as.factor(lowerBound), as.factor(upperBound), total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
    annualRevenues <- rbind(annualRevenues, entry, stringsAsFactors = FALSE)
  }
  names(annualRevenues) <- c("Year", "Start date", "End date", "Advance fees", "Monthly fees", "Final fees", "Monthly rents")
  #write.csv(t(annualRevenues), file=paste("ExcelData/", projectType, "AnnualRevenueData.csv", sep=""))
  
  # Get monthly revenues
  monthlyRevenues <- data.frame()
  operationMonths <- t * 12
  for (i in 1:operationMonths) {
    lowerBound <- sumMonths(now, i - 1)
    upperBound <- sumMonths(lowerBound, 1)
    df <- revenues[lowerBound <= as_date(revenues$Date) & as_date(revenues$Date) < upperBound,]
    total.advance.payments <- sum(df[df$Concept %in% c("Advance fee"),]$Amount)
    total.monthly.payments <- sum(df[df$Concept %in% c("Monthly fee"),]$Amount)
    total.final.payments <- sum(df[df$Concept %in% c("Final fee"),]$Amount)
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    #period <- lowerBound %--% upperBound
    entry <- list(i, as.factor(lowerBound), as.factor(upperBound), total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
    monthlyRevenues <- rbind(monthlyRevenues, entry, stringsAsFactors = FALSE)
  }
  names(monthlyRevenues) <- c("Month", "Start date", "End date", "Advance fees", "Monthly fees", "Final fees", "Monthly rents")
  #write.csv(t(monthlyRevenues), file=paste("ExcelData/", projectType, "MonthlyRevenueData.csv", sep=""))
  
  result <- list(
    revenues,
    projectsPresentValue,
    annualRevenues,
    monthlyRevenues,
    revenuesPlot,
    arrivalsPlot
  )
  names(result) <- c("Revenues", "Projects PV", "Annual revenues", "Monthly revenues", "Revenues chart", "Arrivals chart")
  
  return(result)
}

# ---- Inhouse projects ----

#' Get the present value of each future cash flow that the company will have
#' during the projection period due to the development of inhouse projects
#' acording to the provided revenue scheme.
#'
#' @param t Lifetime of the company
#' Mexican economy:
#' @param y Annual interest rate
#' @param r Inflation rate
#' Project:
#' @param project_type Project type
#' @param lambda Expected number of projects developed in a year
#' Projected development time distribution: 
#' @param sop Support of the discrete distribution of the development time measured in months
#' @param probs Probabilities of the discrete destribution of the development time measured in months 
#' Revenue scheme:
#' @param averageMonthlyRent Project average monthly rent
#' @param sdMonthlyRent Project monthly rent standar deviation
#' @return A list that contains the simulated future revenues, the present value of those revenues, simulated monthly revenues, simulated annual revenues, revenue time series and the arrivals of projects in time.
#' @examples
#' getInhouseProjectsCashFlows(3, 0.07, -0.01, "InhouseProjects", 1.5, c(10,11,12,13,14,15,16), c(2/37,4/37,15/37,10/37,3/37,2/37,1/37), 200000, 30)
getInhouseProjectsCashFlows <- function(t, y, r, projectType, lambda, sop, probs, averageMonthlyRent, sdMonthlyRent) {
  # Calculate monthly effective interest rate
  y12 <- ( 1 + y ) ^ ( 1 / 12) - 1
  
  time <- 0
  arrivalTime <- 0
  revenues <- data.frame()
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
          monthlyRent <- rlnorm(1, mean = log(averageMonthlyRent), sd = 1 / log(sdMonthlyRent)) * (1 + 0.35) ^ arrivalTime
          entry <- list(monthlyRentDate, monthlyRent, TRUE, "Monthly rent")
          revenues <- rbind(revenues, entry, stringsAsFactors = FALSE)
          discountFactor <- 1 / (1 + y) ^ (arrivalTime + (developmentMonths / 12) + (count / 12))
          presentValue <- discountFactor * monthlyRent 
          presentValues <- c(presentValues, presentValue)
          count <- count + 1
        }
      }
    }
  }
  names(revenues) <- c("Date","Amount","Money MXN","Concept")
  breaksVec <- c(seq(from=floor_date(as_date(Sys.time()), unit="month"),
                     to=ceiling_date(max(as_date(revenues$Date)), unit="month"),
                     by="6 months"))
  coloresChidos <- c("hotpink", "gold", "darkorange", "coral4")
  revenuesPlot <- ggplot(data=revenues, aes(x=as_date(revenues$Date), y=revenues$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(revenues$Amount)+1000)) +
    xlim(c(as_date(Sys.time()),max(as_date(revenues$Date)))) +
    labs(title="Revenues", x="Time", y="Money MXN") +
    geom_linerange(aes(x=as_date(revenues$Date), ymax=revenues$Amount, ymin=0, color=revenues$Concept)) +
    guides(color=guide_legend(title="Types of Money MXN")) +
    scale_x_date(breaks = breaksVec, date_minor_breaks = "1 month", limits = c(as_date(Sys.time()), NA)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #geom_linerange(aes(x=as_datetime(revenues$Date), ymax=revenues$Amount, ymin=0), color="#00AFBB") +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Advance fee"),]$Amount), size=1, color=coloresChidos[1]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Monthly fee"),]$Amount), size=1, color=coloresChidos[2]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Final fee"),]$Amount), size=1, color=coloresChidos[3]) +
  #geom_hline(yintercept=mean(revenues[revenues$Concept %in% c("Monthly rent"),]$Amount), size=1, color=coloresChidos[4]) +
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
    df <- revenues[lowerBound <= as_date(revenues$Date) & as_date(revenues$Date) < upperBound,]
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    #period <- lowerBound %--% upperBound
    entry <- list(i, as.factor(lowerBound), as.factor(upperBound), total.rent.payments)
    annualRevenues <- rbind(annualRevenues, entry, stringsAsFactors = FALSE)
  }
  names(annualRevenues) <- c("Year", "Start date", "End date", "Monthly rents")
  #write.csv(t(annualRevenues), file=paste("ExcelData/", projectType, "AnnualRevenueData.csv", sep=""))
  
  # Get monthly revenues
  monthlyRevenues <- data.frame()
  operationMonths <- t * 12
  for (i in 1:operationMonths) {
    lowerBound <- sumMonths(now, i - 1)
    upperBound <- sumMonths(lowerBound, 1)
    df <- revenues[lowerBound <= as_date(revenues$Date) & as_date(revenues$Date) < upperBound,]
    total.advance.payments <- sum(df[df$Concept %in% c("Advance fee"),]$Amount)
    total.monthly.payments <- sum(df[df$Concept %in% c("Monthly fee"),]$Amount)
    total.final.payments <- sum(df[df$Concept %in% c("Final fee"),]$Amount)
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
    period <- lowerBound %--% upperBound
    entry <- list(i, as.factor(lowerBound), as.factor(upperBound), total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
    monthlyRevenues <- rbind(monthlyRevenues, entry, stringsAsFactors = FALSE)
  }
  names(monthlyRevenues) <- c("Month", "Start date", "End date", "Advance fees", "Monthly fees", "Final fees", "Monthly rents")
  #write.csv(t(monthlyRevenues), file=paste("ExcelData/", projectType, "MonthlyRevenueData.csv", sep=""))
  
  result <- list(
    revenues,
    projectsPresentValue,
    annualRevenues,
    monthlyRevenues,
    revenuesPlot,
    arrivalsPlot
  )
  names(result) <- c("Revenues", "Projects PV", "Annual revenues", "Monthly revenues", "Revenues chart", "Arrivals chart")
  
  return(result)
}
