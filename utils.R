library(ggplot2)
library(lubridate)

# ---- Months distribution ----

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

# ---- Utility functions ----
sumMonths <- function(date, months) {
  result <- date + lubridate:::months.numeric(months)
  if (is.na(result)) {
    result <- date + lubridate:::days(3) + lubridate:::months.numeric(months)
    return(result)
  }
  return(result)
}
