library(ggplot2)
library(lubridate)

source("Cash flow analysis/utils.R")

# ---- Inhouse projects algorithm ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate
# Project
projectType="05InhouseProjects" # Project type
lambda=1 # Expected number of projects developed in a year
# Development time distribution
sop=c(   10,   11,    12,    13,   14,   15,   16)
probs=c(2/37,4/37,15/37,10/37,3/37,2/37,1/37)
# Payment scheme
averageMonthlyRent=400000 # Project average monthly rent
sdMonthlyRent=100000 # Project monthly rent standar deviation

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
write.csv(t(annualRevenues), file=paste(projectType,"AnnualRevenueData.csv",sep=""))

# Get monthly revenues
monthlyRevenues <- data.frame()
operationMonths <- t * 12
for (i in 1:operationMonths) {
  lowerBound <- sumMonths(now, i - 1)
  upperBound <- sumMonths(lowerBound, 1)
  df <- positiveFlows[lowerBound <= as_date(positiveFlows$Date) & as_date(positiveFlows$Date) < upperBound,]
  total.advance.payments <- sum(df[df$Concept %in% c("Advance fee"),]$Amount)
  total.monthly.payments <- sum(df[df$Concept %in% c("Monthly fee"),]$Amount)
  total.final.payments <- sum(df[df$Concept %in% c("Final fee"),]$Amount)
  total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent"),]$Amount)
  period <- lowerBound %--% upperBound
  entry <- list(i, as.factor(period), total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
  monthlyRevenues <- rbind(monthlyRevenues, entry, stringsAsFactors = FALSE)
}
names(monthlyRevenues) <- c("Month", "Period","Advance fees","Monthly fees","Final fees","Monthly rents")
write.csv(t(monthlyRevenues), file=paste(projectType,"MonthlyRevenueData.csv",sep=""))

result <- list(
  positiveFlows,
  projectsPresentValue,
  annualRevenues,
  monthlyRevenues,
  revenuesPlot,
  arrivalsPlot
)
names(result) <- c("Flows", "Projects PV", "Annual revenues", "Monthly revenues", "Revenues", "Arrivals")






