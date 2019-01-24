#' Code more efficient
#' These functions do not generate the data frame with all the cash flows in time

# ---- Normal projects ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate
# Princing of the project
average.price=1600000 # Expected price of the project
sd.price=200000 # Standar deviation of the price of the project
minimum.price=500000 # Minimum price accepted for the project
average.development.months=13 # Expected development time of the project measured in months
lambda.po=1 # Expected number of projects developed in a year
# Payment scheme
advance.charge.rate=0.25 # Percentage of the final price that is paid at the begining of the project
during.development.charge.rate=0.5 # Percentage of the final price that is paid during the development of the project
final.charge.rate=0.25 # Percentage of the final price that is paid at the end of the development of the project
monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance

# Calculate monthly effective interest rate
y12 <- (1+y)^(1/12)-1

values <- c()
for (j in 1:10000) {
  time <- 0
  arrival.time <- 0
  present.values <- c()
  # Simulate projects arrivals
  while (time < t) {
    interarrival.time <- rexp(n=1, rate=lambda.po)
    time <- time + interarrival.time 
    if (time < t) {
      arrival.time <- arrival.time + interarrival.time
      # Compute the present value of the project that just came in
      price <- rnorm(1, mean=average.price, sd=sd.price)-rgamma(1, shape=2, rate=0.6)*1000
      price <- max(minimum.price, price)
      # Adjust price with inflation rate
      adjusted.price <- price*(1+r)^arrival.time
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
  projects.present.value <- sum(present.values)
  values <- c(values, projects.present.value)
}
mean(values)

# ---- Inhouse projects ----

t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate
# Project details
lambda.po=5 # Expected number of projects developed in a year
average.development.months=14 # Expected development time of the project measured in months
# Revenue scheme
average.monthly.rent=400000 # Expected value of the monthly rent that the project will generate
sd.monthly.rent=100000 # Standar deviation of the monthly rent that the project will generate

# Calculate monthly effective interest rate
y12 <- (1+y)^(1/12)-1

values <- c()
for (j in 1:10000) {
  time <- 0
  arrival.time <- 0
  present.values <- c()
  # Simulate projects arrivals
  while (time < t) {
    interarrival.time <- rexp(n=1, rate=lambda.po)
    time <- time + interarrival.time 
    if (time < t) {
      arrival.time <- arrival.time + interarrival.time
      # Get development time
      development.time <- rlnorm(1, meanlog=0, sdlog=0.25)*average.development.months
      development.months <- ceiling(development.time)
      time.to.end <- t - (arrival.time + development.months/12)
      time.to.end <- max(0, time.to.end)
      months.to.end <- ceiling(time.to.end*12)
      monthly.rent <- rnorm(1, mean=average.monthly.rent, sd=sd.monthly.rent)
      monthly.rent <- max(0, monthly.rent)
      # Compute present values
      discount.factor <- (1/(1+y))^arrival.time
      monthly.rents.present.value <- discount.factor*monthly.rent*((1-(1/(1+y12))^(months.to.end))/y12)
      present.value <- monthly.rents.present.value
      present.values <- c(present.values, present.value)
    }
  }
  projects.present.value <- sum(present.values)
  values <- c(values, projects.present.value)
}
mean(values)
