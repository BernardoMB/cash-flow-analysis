library(ggplot2)
library(lubridate)

## Using gama months distribution
# averageDevelopmentMonths <- 12
# sdDevelopmentMonths <- 3
# alpha <- sdDevelopmentMonths ^ 2
# beta <- sdDevelopmentMonths ^ 2 / averageDevelopmentMonths
# vals <- rgamma(n = 10000, shape = alpha, rate = beta)
# vals <- ceiling(vals)
# hist(vals)
# mean(vals)
# sqrt(var(vals))

## Using lognormal months distribution
# averageDevelopmentMonths <- 12
# sdDevelopmentMonths <- 4
# mu <- log(averageDevelopmentMonths)
# s <- log(0.5 * (sqrt(((averageDevelopmentMonths ^ 2) + (4 * sdDevelopmentMonths)) / averageDevelopmentMonths ^ 2) + 1))
# vals <- rlnorm(n = 10000, meanlog = mu, sdlog = s)
# #vals <- ceiling(vals)
# hist(vals)
# mean(vals)
# sd(vals)

## Using custom months distribution
sop <- c(   2,   3,    4,    5,   6,   7,   8)
probs <- c(2/37,4/37,15/37,10/37,3/37,2/37,1/37)
# mean <- sop %*% probs
# secondMoment <- sop^2 %*% probs
# var <- secondMoment - mean ^ 2
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
vals <- c()
for (i in 1:10000) {
  val <- sampleFromMonthsDistribution(sop = sop, probs = probs)
  vals <- c(vals, val)
}
ggplot(data.frame(vals), aes(x=vals)) + 
  geom_histogram(color="black", fill="white")

# ---- Price distribution ----
vals <- rlnorm(10000, mean = log(50000), sd = 1 / log(20000))
ggplot(data.frame(vals), aes(x=vals)) + 
  geom_histogram(color="black", fill="white")

## Utility functions
sumMonths <- function(date, months) {
  result <- date + lubridate:::months.numeric(months)
  if (is.na(result)) {
    result <- date + lubridate:::days(3) + lubridate:::months.numeric(months)
    return(result)
  }
  return(result)
}

# ---- Algorithm improvements ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate
# Project
projectType="02SmallProjects" # Project type
averagePrice=50000 # Expected price of the project
sdPrice=20000 # Standar deviation of the price of the project
lambda=5 # Expected number of projects developed in a year
# Development time distribution
sop=c(   2,   3,    4,    5,   6,   7,   8)
probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37)
# Payment scheme
advanceFeeRate=0.25 # Percentage of the final price that is paid at the begining of the project
monthlyFeeRate=0.5 # Percentage of the final price that is paid during the development of the project
finalFeeRate=0.25 # Percentage of the final price that is paid at the end of the development of the project
monthlyRentRate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance

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
write.csv(t(annualRevenues), file=paste(projectType,"RevenueData.csv",sep=""))
result <- list(
  positiveFlows,
  projectsPresentValue,
  annualRevenues,
  revenuesPlot,
  arrivalsPlot
)
names(result) <- c("Flows", "Projects PV", "Annual revenues", "Revenues", "Arrivals")






