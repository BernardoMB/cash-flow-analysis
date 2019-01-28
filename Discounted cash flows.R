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

getProjectPresentValue <- function(
  t,# Lifetime of the company
  # Mexican economy
  y, # Annual interest rate
  r, # Inflation rate
  # Princing of the project
  project.type, # Project type
  average.price, # Expected price of the project
  sd.price, # Standar deviation of the price of the project
  minimum.price, # Minimum price accepted for the project
  average.development.months, # Expected development time of the project measured in months
  lambda.po, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate, # Percentage of the final price that is paid during the development of the project
  final.charge.rate, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate # Percentaje of the price of the project to be charged monthly for project maintenance
) {
  # Calculate monthly effective interest rate
  y12 <- (1+y)^(1/12)-1
  
  time <- 0
  arrival.time <- 0
  interarrivals <- c()
  arrivals <- c()
  positive.flows <- data.frame()
  negative.flows <- data.frame()
  adjusted.prices <- c()
  present.values <- c()
  # Simulate projects arrivals
  while (time < t) {
    interarrival.time <- rexp(n=1, rate=lambda.po)
    time <- time + interarrival.time 
    if (time < t) {
      interarrivals <- c(interarrivals, interarrival.time)
      arrival.time <- arrival.time + interarrival.time
      arrivals <- c(arrivals, arrival.time)
      # Compute the present value of the project that just came in
      price <- rnorm(1, mean=average.price, sd=sd.price)-rgamma(1, shape=2, rate=0.6)*1000
      price <- max(minimum.price, price)
      # Adjust price with inflation rate
      adjusted.price <- price*(1+r)^arrival.time
      adjusted.prices <- c(adjusted.prices, adjusted.price)
      # Payment scheme
      advance.charge <- adjusted.price*advance.charge.rate
      during.development.charge <- adjusted.price*during.development.charge.rate
      final.charge <- adjusted.price*final.charge.rate
      monthly.rent <- adjusted.price*monthly.rent.rate
      # Simulate development time in months
      development.time <- rlnorm(1, meanlog=0, sdlog=0.25)*average.development.months
      development.months <- ceiling(development.time)
      monthly.payment <- during.development.charge/development.months
      time.to.end <- t - (arrival.time + development.months/12)
      time.to.end <- max(0, time.to.end)
      months.to.end <- ceiling(time.to.end*12)
      # Populate cash flows collections
      now <- as_datetime(Sys.time())
      arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
      arrival.date <- now + seconds(arrival.time.in.seconds)
      entry <- list(arrival.date, advance.charge, TRUE, "Advance charge")
      positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
        for (l in 1:development.months) {
        monthly.payment.date <- arrival.date + months(l)
        if (is.na(monthly.payment.date)) {
          monthly.payment.date <- arrival.date - days(3) + months(l)
          if (is.na(monthly.payment.date)) {
            print("No mames no puede ser")
          }
        }
        entry <- list(monthly.payment.date, monthly.payment, TRUE, "Monthly charge")
        positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      }
      end.date <- arrival.date + months(development.months)
      if (is.na(end.date)) {
        end.date <- arrival.date - days(3) + months(development.months)
        if (is.na(end.date)) {
          print("No mames no puede ser")
        }
      }
      entry <- list(end.date, final.charge, TRUE, "Final charge")
      positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      for (s in 1:months.to.end) {
        monthly.rent.payment.date <- arrival.date + months(development.months + s)
        if (is.na(monthly.rent.payment.date)) {
          monthly.rent.payment.date <- arrival.date - days(3) + months(development.months + s)
          if (is.na(monthly.rent.payment.date)) {
            print("No mames no puede ser")
          }
        }
        entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly rent payment")
        positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      }
      names(positive.flows) <- c("Date","Amount","Income","Concept")
      #names(negative.flows) <- c("Date","Amount","Income","Concept")
      # Compute present values based on payment scheme
      discount.factor <- (1/(1+y))^arrival.time
      fractional.annuity <- ((1-(1/(1+y12))^development.months)/y12)
      advance.charge.present.value <- discount.factor*advance.charge
      monthly.charges.present.value <- discount.factor*monthly.payment*fractional.annuity
      final.payment.present.value <- discount.factor*((1/(1+y))^(development.time/12))*final.charge
      monthly.rents.present.value <- discount.factor*((1/(1+y))^(development.time/12))*monthly.rent*((1-(1/(1+y12))^(months.to.end))/y12)
      # Compute total present value
      present.value <- advance.charge.present.value + monthly.charges.present.value + final.payment.present.value + monthly.rents.present.value
      present.values <- c(present.values, present.value)
    }
  }
  
  revenues.plot <- ggplot(data=positive.flows, aes(x=as_datetime(positive.flows$Date), y=positive.flows$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(positive.flows$Amount)+1000)) +
    xlim(c(as_datetime(Sys.time()),max(as_datetime(positive.flows$Date)))) +
    #geom_hline(yintercept=mean(positive.flows$Amount), size=1, color="cyan") +
    #geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0), color="#00AFBB") +
    geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0, color=positive.flows$Concept)) +
    labs(title="Revenues", x="Time (years)", y="Income") +
    guides(color=guide_legend(title="Types of income"))
  
  arrivals.df <- positive.flows[positive.flows$Concept %in% c("Advance charge"),]
  arrivals.plot <- ggplot(data=arrivals.df, aes(x=as_datetime(arrivals.df$Date), y=c(0))) + 
    ylim(c(0,0)) +
    labs(title="Project arrivals", x="Time", y="") +
    theme(aspect.ratio=0.1, 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_hline(yintercept=0, size=0.5, color="cyan") +
    geom_point(shape=4, size=2) 
  
  projects.present.value <- sum(present.values)
  
  # Export anual data
  now <- as_datetime(Sys.time())
  count <- 1
  # Get revenues for each year
  revenues <- data.frame()
  repeat {
    then <- now + years(count-1)
    year <- lubridate::year(as_datetime(then))
    df <- positive.flows[year(as_datetime(positive.flows$Date)) == year,]
    if (length(df[,1]) == 0) {
      break
    }
    total.advance.payments <- sum(df[df$Concept %in% c("Advance charge"),]$Amount)
    total.monthly.payments <- sum(df[df$Concept %in% c("Monthly charge"),]$Amount)
    total.final.payments <- sum(df[df$Concept %in% c("Final charge"),]$Amount)
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly rent payment"),]$Amount)
    entry <- list(year, total.advance.payments, total.monthly.payments, total.final.payments, total.rent.payments)
    revenues <- rbind(revenues, entry)
    names(revenues) <- c("Year","Advance","Development","Final","Rent")
    count <- count + 1
  }
  write.csv(revenues, file=paste(project.type,"AnnualRevenueData.csv",sep=""))
  
  return(list(
    positive.flows,
    negative.flows,
    revenues.plot,
    arrivals.plot,
    projects.present.value
  ))
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

getInhouseProjectPresentValue <- function(
  t, # Lifetime of the company
  # Mexican economy
  y, # Annual interest rate
  r, # Inflation rate
  # Project details
  lambda.po, # Expected number of projects developed in a year
  average.development.months, # Expected development time of the project measured in months
  # Revenue scheme
  average.monthly.rent, # Expected value of the monthly rent that the project will generate
  sd.monthly.rent # Standar deviation of the monthly rent that the project will generate
) {
  # Calculate monthly effective interest rate
  y12 <- (1+y)^(1/12)-1
  
  time <- 0
  arrival.time <- 0
  interarrivals <- c()
  arrivals <- c()
  adjusted.prices <- c()
  positive.flows <- data.frame()
  negative.flows <- data.frame()
  present.values <- c()
  arrival.dates <- c()
  # Simulate projects arrivals
  while (time < t) {
    interarrival.time <- rexp(n=1, rate=lambda.po)
    time <- time + interarrival.time 
    if (time < t) {
      interarrivals <- c(interarrivals, interarrival.time)
      arrival.time <- arrival.time + interarrival.time
      arrivals <- c(arrivals, arrival.time)
      # Get development time
      development.time <- rlnorm(1, meanlog=0, sdlog=0.25)*average.development.months
      development.months <- ceiling(development.time)
      time.to.end <- t - (arrival.time + development.months/12)
      time.to.end <- max(0, time.to.end)
      months.to.end <- ceiling(time.to.end*12)
      monthly.rent <- rnorm(1, mean=average.monthly.rent, sd=sd.monthly.rent)
      monthly.rent <- max(0, monthly.rent)
      # Populate cash flows collections
      now <- as_datetime(Sys.time())
      arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
      arrival.date <- now + seconds(arrival.time.in.seconds)
      arrival.dates <- c(arrival.dates, arrival.date)
      for (s in 1:months.to.end) {
        monthly.rent.payment.date <- arrival.date + months(development.months + s)
        if (is.na(monthly.rent.payment.date)) {
          monthly.rent.payment.date <- arrival.date - days(3) + months(development.months + s)
          if (is.na(monthly.rent.payment.date)) {
            print("No mames no puede ser")
          }
        }
        entry <- list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly revenue of inhouse project")
        positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      }
      names(positive.flows) <- c("Date","Amount","Income","Concept")
      #names(negative.flows) <- c("Date","Amount","Income","Concept")
      # Compute present values
      discount.factor <- (1/(1+y))^arrival.time
      monthly.rents.present.value <- discount.factor*monthly.rent*((1-(1/(1+y12))^(months.to.end))/y12)
      present.value <- monthly.rents.present.value
      present.values <- c(present.values, present.value)
    }
  }
  
  revenues.plot <- ggplot(data=positive.flows, aes(x=as_datetime(positive.flows$Date), y=positive.flows$Amount)) +
    geom_point(color="blue", size=0.5) + 
    ylim(c(0,max(positive.flows$Amount)+1000)) +
    xlim(c(as_datetime(Sys.time()),max(as_datetime(positive.flows$Date)))) +
    #geom_hline(yintercept=mean(positive.flows$Amount), size=1, color="cyan") +
    #geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0), color="#00AFBB") +
    geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0, color=positive.flows$Concept)) +
    labs(title="Revenues", x="Time (years)", y="Income") +
    guides(color=guide_legend(title="Types of income"))
  
  arrivals.df <- data.frame(arrival.dates)
  names(arrivals.df) <- c("Date")
  
  arrivals.plot <- ggplot(data=arrivals.df, aes(x=as_datetime(arrivals.df$Date), y=c(0))) + 
    ylim(c(0,0)) +
    labs(title="Project arrivals", x="Time", y="") +
    theme(aspect.ratio=0.1, 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    geom_hline(yintercept=0, size=0.5, color="cyan") +
    geom_point(shape=4, size=2) 
  
  projects.present.value <- sum(present.values)
  
  # Export anual data
  now <- as_datetime(Sys.time())
  count <- 1
  # Get revenues for each year
  revenues <- data.frame()
  repeat {
    then <- now + years(count-1)
    year <- lubridate::year(as_datetime(then))
    df <- positive.flows[year(as_datetime(positive.flows$Date)) == year,]
    if (year > lubridate::year(now + years(t))) {
      break
    }
    total.rent.payments <- sum(df[df$Concept %in% c("Monthly revenue of inhouse project"),]$Amount)
    entry <- list(year, total.rent.payments)
    revenues <- rbind(revenues, entry)
    names(revenues) <- c("Year","Rent")
    count <- count + 1
  }
  write.csv(revenues, file=paste("05InhouseProjects","AnnualRevenueData.csv",sep=""))
  
  return(list(
    positive.flows,
    negative.flows,
    revenues.plot,
    arrivals.plot,
    projects.present.value
  ))
}

# ---- Simulation ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate

valueSmallProjects <- getProjectsCashFlows(
  t=5, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
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

value.landings <- getProjectPresentValue(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  project.type="01Landings", # Project type
  average.price=10000, # Expected price of the project
  sd.price=2000, # Standar deviation of the price of the project
  minimum.price=7000, # Minimum price accepted for the project
  average.development.months=1, # Expected development time of the project measured in months
  lambda.po=12*2, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate=1, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate=0, # Percentage of the final price that is paid during the development of the project
  final.charge.rate=0, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate=0.025 # Percentaje of the price of the project to be charged monthly for project maintenance
)

value.small.projects <- getProjectPresentValue(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  project.type="02SmallProjects", # Project type
  average.price=50000, # Expected price of the project
  sd.price=20000, # Standar deviation of the price of the project
  minimum.price=25000, # Minimum price accepted for the project
  average.development.months=4, # Expected development time of the project measured in months
  lambda.po=5, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate=0.25, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate=0.5, # Percentage of the final price that is paid during the development of the project
  final.charge.rate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

value.large.projects <- getProjectPresentValue(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  project.type="03LargeProjects", # Project type
  average.price=200000, # Expected price of the project
  sd.price=70000, # Standar deviation of the price of the project
  minimum.price=100000, # Minimum price accepted for the project
  average.development.months=7, # Expected development time of the project measured in months
  lambda.po=2, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate=0.25, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate=0.5, # Percentage of the final price that is paid during the development of the project
  final.charge.rate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

value.enterprice.projects <- getProjectPresentValue(
  t=t, # Lifetime of the company
  # Mexican economy
  y=y, # Annual interest rate
  r=r, # Inflation rate
  # Princing of the project
  project.type="04EnterpriceProjects", # Project type
  average.price=1600000, # Expected price of the project
  sd.price=200000, # Standar deviation of the price of the project
  minimum.price=500000, # Minimum price accepted for the project
  average.development.months=13, # Expected development time of the project measured in months
  lambda.po=1, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate=0.25, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate=0.5, # Percentage of the final price that is paid during the development of the project
  final.charge.rate=0.25, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance
)

value.inhouse.projects <- getInhouseProjectPresentValue(
  t=t, # Lifetime of the company
  # Mexican economy
  y=t, # Annual interest rate
  r=r, # Inflation rate
  # Project details
  lambda.po=5, # Expected number of projects developed in a year
  average.development.months=14, # Expected development time of the project measured in months
  # Revenue scheme
  average.monthly.rent=400000, # Expected value of the monthly rent that the project will generate
  sd.monthly.rent=100000 # Standar deviation of the monthly rent that the project will generate
)

valueInhouseProjects <- getInhouseProjectsCashFlows(
  t=5, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
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

total.value <- value.landings[[5]] + 
  value.small.projects[[5]] + 
  value.large.projects[[5]] + 
  value.enterprice.projects[[5]] + 
  value.inhouse.projects[[5]]

# Costs
building <- 60000
intenet <- 1300
water <- 40*2*4
parking <- 2000*5
salaries <- 70000*7 + 40000
monthly.cost <- building + intenet + water + parking + salaries
initial.cost <- 200000
r <- 0.05 # Anual inflation rate
t <- 1 # Years
y <- 0.10 # Annual interest rate
y12 <- (1+y)^(1/12)-1 # Monthly effective interest rate
cost.present.value <- 0
annuity.sum <- 0
months2 <- 12*t
for (k in 1:months2) {
  annuity.sum <- annuity.sum + 1/(1+y12)^k
}
annuity.sum
monthly.cost.present.value <- monthly.cost*annuity.sum
total.cost <- initial.cost + monthly.cost.present.value