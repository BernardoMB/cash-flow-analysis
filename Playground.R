library(lubridate)
library(ggplot2)

# ---- Simulate cash flows from developing a type of project ----

# vals <- c()
# for (n in 1:10000) {
  
t=10 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate
# Princing of the project
average.price=10000 # Expected price of the project
sd.price=100 # Standar deviation of the price of the project
minimum.price=500 # Minimum price accepted for the project
average.development.months=4 # Expected development time of the project measured in months
lambda.po=5 # Expected number of projects developed in a year
# Payment scheme
advance.charge.rate=0.25 # Percentage of the final price that is paid at the begining of the project
during.development.charge.rate=0.5 # Percentage of the final price that is paid during the development of the project
final.charge.rate=0.25 # Percentage of the final price that is paid at the end of the development of the project
monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance

positive.flows <- list()
negative.flows <- list()

positive.flows.df <- data.frame()
negative.flows.df <- data.frame()

# Calculate monthly effective interest rate
y12 <- (1+y)^(1/12)-1

time <- 0
arrival.time <- 0
interarrivals <- c()
arrivals <- c()
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
    # Populate cashflows date 
    now <- as_datetime(Sys.time())
    arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
    arrival.date <- now + seconds(arrival.time.in.seconds)
    index <- length(positive.flows) + 1
    entry <- list(arrival.date, advance.charge, TRUE, "Advance charge")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
    for (l in 1:months) {
      monthly.payment.date <- arrival.date + months(l)
      index <- length(positive.flows) + 1
      entry <- list(monthly.payment.date, monthly.payment, TRUE, "During development monthly payment")
      positive.flows[[index]] <- entry
      positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
    }
    end.date <- arrival.date + months(months)
    index <- length(positive.flows) + 1
    entry <- list(end.date, final.charge, TRUE, "Final charge")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
    for (s in 1:months.to.end) {
      monthly.rent.payment.date <- arrival.date + months(months + s)
      index <- length(positive.flows) + 1
      entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly rent payment")
      positive.flows[[index]] <- entry
      positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
    }
    names(positive.flows.df) <- c("Date","Amount","Income","Concept")
    #names(negative.flows.df) <- c("Date","Amount","Income","Concept")
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
sum(present.values)

# vals <- c(vals, sum(present.values))
# }
# mean(vals)

ggplot(data=positive.flows.df, aes(x=as_datetime(positive.flows.df$Date), y=positive.flows.df$Amount)) +
  geom_point(color="blue", size=0.5) + 
  ylim(c(0,max(positive.flows.df$Amount)+1000)) +
  xlim(c(as_datetime(Sys.time()),max(as_datetime(positive.flows.df$Date)))) +
  geom_hline(yintercept=mean(positive.flows.df$Amount), size=1, color="cyan") +
  #geom_linerange(aes(x=as_datetime(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0), color="#00AFBB") +
  geom_linerange(aes(x=as_datetime(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0, color=positive.flows.df$Concept)) +
  labs(x="Time (years)", y="Money")


# ---- Simulate cash flows from developing inhouse projects ----

t <- 10 # Lifetime of the company
# Mexican economy
y <- 0.8 # Annual interest rate
r <- -0.02 # Inflation rate
# Project details
lambda.po <- 1 # Expected number of projects developed in a year
average.development.months <- 14 # Expected development time of the project measured in months
# Revenue scheme
average.monthly.rent <- 400000 # Expected value of the monthly rent that the project will generate
sd.monthly.rent <- 100000 # Standar deviation of the monthly rent that the project will generate

positive.flows <- list()
negative.flows <- list()

positive.flows.df <- data.frame()
negative.flows.df <- data.frame()

# Calculate monthly effective interest rate
y12 <- (1+y)^(1/12)-1

time <- 0
arrival.time <- 0
interarrivals <- c()
arrivals <- c()
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
    for (s in 1:months.to.end) {
      monthly.rent.payment.date <- arrival.date + months(months + s)
      index <- length(positive.flows) + 1
      entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly revenue of inhouse project")
      positive.flows[[index]] <- entry
      positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
    }
    names(positive.flows.df) <- c("Date","Amount","Income","Concept")
    #names(negative.flows.df) <- c("Date","Amount","Income","Concept")
    # Compute present values
    discount.factor <- (1/(1+y))^arrival.time
    monthly.rents.present.value <- discount.factor*monthly.rent*((1-(1/(1+y12))^(months.to.end))/y12)
    present.value <- monthly.rents.present.value
    present.values <- c(present.values, present.value)
  }
}
sum(present.values)

ggplot(data=positive.flows.df, aes(x=as_datetime(positive.flows.df$Date), y=positive.flows.df$Amount)) +
  geom_point(color="blue", size=0.5) + 
  ylim(c(0,max(positive.flows.df$Amount)+1000)) +
  xlim(c(as_datetime(Sys.time()),max(as_datetime(positive.flows.df$Date)))) +
  geom_hline(yintercept=mean(positive.flows.df$Amount), size=1, color="cyan") +
  geom_linerange(aes(x=as_datetime(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0, color=positive.flows.df$Concept)) +
  labs(x="Time (years)", y="Money")

# ---- Simulate cash flows of a single project ----

t=10 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rates
r=-0.02 # Inflation rate
# Princing of the project
average.price=10000 # Expected price of the project
sd.price=100 # Standar deviation of the price of the project
minimum.price=500 # Minimum price accepted for the project
average.development.months=4 # Expected development time of the project measured in months
lambda.po=5 # Expected number of projects developed in a year
# Payment scheme
advance.charge.rate=0.25 # Percentage of the final price that is paid at the begining of the project
during.development.charge.rate=0.5 # Percentage of the final price that is paid during the development of the project
final.charge.rate=0.25 # Percentage of the final price that is paid at the end of the development of the project
monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance

positive.flows <- list()
negative.flows <- list()

positive.flows.df <- data.frame()
negative.flows.df <- data.frame()

# Calculate monthly effective interest rate
y12 <- (1+y)^(1/12)-1

time <- 0
arrival.time <- 0
interarrivals <- c()
arrivals <- c()
adjusted.prices <- c()
present.values <- c()

#interarrival.time <- rexp(n=1, rate=lambda.po)
interarrival.time <- 9.5
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
  # Populate cashflows date 
  now <- as_datetime(Sys.time())
  arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
  arrival.date <- now + seconds(arrival.time.in.seconds)
  index <- length(positive.flows) + 1
  entry <- list(arrival.date, advance.charge, TRUE, "Advance charge")
  positive.flows[[index]] <- entry
  positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  for (l in 1:months) {
    monthly.payment.date <- arrival.date + months(l)
    index <- length(positive.flows) + 1
    entry <- list(monthly.payment.date, monthly.payment, TRUE, "During development monthly payment")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  }
  end.date <- arrival.date + months(months)
  index <- length(positive.flows) + 1
  entry <- list(end.date, final.charge, TRUE, "Final charge")
  positive.flows[[index]] <- entry
  positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  for (s in 1:months.to.end) {
    monthly.rent.payment.date <- arrival.date + months(months + s)
    index <- length(positive.flows) + 1
    entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly rent payment")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  }
  names(positive.flows.df) <- c("Date","Amount","Income","Concept")
  #names(negative.flows.df) <- c("Date","Amount","Income","Concept")
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

ggplot(data=positive.flows.df, aes(x=as_datetime(positive.flows.df$Date), y=positive.flows.df$Amount)) +
  geom_point(color="blue", size=0.5) + 
  ylim(c(0,max(positive.flows.df$Amount)+1000)) +
  #xlim(c(as_datetime(Sys.time()),as_datetime(Sys.time()) + years(10))) +
  xlim(c(as_datetime(Sys.time()),max(as_datetime(positive.flows.df$Date)))) +
  #geom_hline(yintercept=mean(positive.flows.df$Amount), size=1, color="cyan") +
  geom_linerange(aes(x=as_datetime(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0), color="#00AFBB") +
  labs(x="Time (years)", y="Money")