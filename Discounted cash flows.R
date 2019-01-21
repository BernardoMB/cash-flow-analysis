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

# ---- Functions ----

getProjectPresentValue <- function(
  t,# Lifetime of the company
  # Mexican economy
  y, # Annual interest rate
  r, # Inflation rate
  # Princing of the project
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
      months <- ceiling(development.time)
      monthly.payment <- during.development.charge/months
      time.to.end <- t - (arrival.time + months/12)
      time.to.end <- max(0, time.to.end)
      months.to.end <- ceiling(time.to.end*12)
      # Populate cash flows collections
      now <- as_datetime(Sys.time())
      arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
      arrival.date <- now + seconds(arrival.time.in.seconds)
      entry <- list(arrival.date, advance.charge, TRUE, "Advance charge")
      positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      for (l in 1:months) {
        monthly.payment.date <- arrival.date + months(l)
        entry <- list(monthly.payment.date, monthly.payment, TRUE, "During development monthly payment")
        positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      }
      end.date <- arrival.date + months(months)
      entry <- list(end.date, final.charge, TRUE, "Final charge")
      positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      for (s in 1:months.to.end) {
        monthly.rent.payment.date <- arrival.date + months(months + s)
        entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly rent payment")
        positive.flows <- rbind(positive.flows, entry, stringsAsFactors=FALSE)
      }
      names(positive.flows) <- c("Date","Amount","Income","Concept")
      #names(negative.flows) <- c("Date","Amount","Income","Concept")
      # Compute present values based on payment scheme
      discount.factor <- (1/(1+y))^arrival.time
      fractional.annuity <- ((1-(1/(1+y12))^months)/y12)
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
  
  return(list(
    positive.flows,
    negative.flows,
    revenues.plot,
    arrivals.plot,
    projects.present.value
  ))
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
      months <- ceiling(development.time)
      time.to.end <- t - arrival.time - months/12
      months.to.end <- ceiling(time.to.end*12)
      monthly.rent <- rnorm(1, mean=average.monthly.rent, sd=sd.monthly.rent)
      monthly.rent <- max(0, monthly.rent)
      # Populate cash flows collections
      now <- as_datetime(Sys.time())
      arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
      arrival.date <- now + seconds(arrival.time.in.seconds)
      arrival.dates <- c(arrival.dates, arrival.date)
      for (s in 1:months.to.end) {
        monthly.rent.payment.date <- arrival.date + months(months + s)
        entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly revenue of inhouse project")
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
  
  return(list(
    positive.flows,
    negative.flows,
    revenues.plot,
    arrivals.plot,
    projects.present.value
  ))
}

# ---- Simulation ----

value.landings <- getProjectPresentValue(
  t=10, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
  # Princing of the project
  average.price=10000, # Expected price of the project
  sd.price=2000, # Standar deviation of the price of the project
  minimum.price=7000, # Minimum price accepted for the project
  average.development.months=1, # Expected development time of the project measured in months
  lambda.po=12*2, # Expected number of projects developed in a year
  # Payment scheme
  advance.charge.rate=1, # Percentage of the final price that is paid at the begining of the project
  during.development.charge.rate=0, # Percentage of the final price that is paid during the development of the project
  final.charge.rate=0, # Percentage of the final price that is paid at the end of the development of the project
  monthly.rent.rate=0 # Percentaje of the price of the project to be charged monthly for project maintenance
)

value.small.projects <- getProjectPresentValue(
  t=10, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
  # Princing of the project
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
  t=10, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
  # Princing of the project
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
  t=10, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
  # Princing of the project
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
  t=10, # Lifetime of the company
  # Mexican economy
  y=0.1, # Annual interest rate
  r=-0.02, # Inflation rate
  # Project details
  lambda.po=5, # Expected number of projects developed in a year
  average.development.months=14, # Expected development time of the project measured in months
  # Revenue scheme
  average.monthly.rent=400000, # Expected value of the monthly rent that the project will generate
  sd.monthly.rent=100000 # Standar deviation of the monthly rent that the project will generate
)

total.value <- value.landings + value.small.projects + value.large.projects + value.enterprice.projects + value.inhouse.projects

# Costs
building <- 60000
intenet <- 1300
water <- 40*2*4
parking <- 2000*5
salaries <- 94000*7 + 40000
monthly.cost <- building + intenet + water + parking + salaries
initial.cost <- 200000
r <- 0.05 # Anual inflation rate
t <- 1 # Years
y <- 0.08 # Annual interest rate
y12 <- (1+y)^(1/12)-1 # Monthly effective interest rate
cost.present.value <- 0
# TODO: Consider inflation rates
monthly.cost.present.value <- monthly.cost*((1-(1/(1+y12))^(12*t))/y12)
total.cost <- initial.cost + monthly.cost.present.value


