library(lubridate)
library(ggplot2)

# ---- Simulate cash flows from developing a type of project ----

# vals <- c()
# for (n in 1:10000) {
  
t=5 # Lifetime of the company
# Mexican economy
y=0.1 # Annual interest rate
r=-0.02 # Inflation rate

# Princing of the project
average.price=50000 # Expected price of the project
sd.price=20000 # Standar deviation of the price of the project
minimum.price=25000 # Minimum price accepted for the project
average.development.months=4 # Expected development time of the project measured in months
lambda.po=5 # Expected number of projects developed in a year
# Payment scheme
advance.charge.rate=0.25 # Percentage of the final price that is paid at the begining of the project
during.development.charge.rate=0.5 # Percentage of the final price that is paid during the development of the project
final.charge.rate=0.25 # Percentage of the final price that is paid at the end of the development of the project
monthly.rent.rate=0.05 # Percentaje of the price of the project to be charged monthly for project maintenance

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

#write.csv(positive.flows, file = "myData.csv")

projects.present.value <- sum(present.values)

# vals <- c(vals, projects.present.value)
# }
# mean(vals)

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
  theme(
    #aspect.ratio=0.1, 
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  geom_hline(yintercept=0, size=0.5, color="cyan") +
  geom_point(shape=4, size=2) 






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
write.csv(revenues, file=paste("","RevenueData.csv",sep=""))





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
    for (s in 1:months.to.end) {
      monthly.rent.payment.date <- arrival.date + months(development.months + s)
      if (is.na(monthly.rent.payment.date)) {
        monthly.rent.payment.date <- arrival.date - days(3) + months(development.months + s)
        if (is.na(monthly.rent.payment.date)) {
          print("No mames no puede ser")
        }
      }
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
  #geom_hline(yintercept=mean(positive.flows$Amount), size=1, color="cyan") +
  #geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0), color="#00AFBB") +
  geom_linerange(aes(x=as_datetime(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0, color=positive.flows.df$Concept)) +
  labs(title="Revenues", x="Time (years)", y="Income") +
  guides(color=guide_legend(title="Types of income"))

# ---- Simulate cash flows of a single project ----

t=1 # Lifetime of the company
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
interarrival.time <- 0.5
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
  # Populate cashflows date 
  now <- as_date(Sys.time())
  arrival.time.in.seconds <- as.numeric(duration(arrival.time, unit="years"))
  arrival.date <- as_date(now + seconds(arrival.time.in.seconds))
  index <- length(positive.flows) + 1
  entry <- list(arrival.date, advance.charge, TRUE, "Advance charge")
  positive.flows[[index]] <- entry
  positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  for (l in 1:development.months) {
    monthly.payment.date <- arrival.date + months(l)
    if (is.na(monthly.payment.date)) {
      monthly.payment.date <- arrival.date - days(3) + months(l)
      if (is.na(monthly.payment.date)) {
        print("No mames no puede ser")
      }
    }
    index <- length(positive.flows) + 1
    entry <- list(monthly.payment.date, monthly.payment, TRUE, "During development monthly payment")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  }
  end.date <- arrival.date + months(development.months)
  if (is.na(end.date)) {
    end.date <- arrival.date - days(3) + months(development.months)
    if (is.na(end.date)) {
      print("No mames no puede ser")
    }
  }
  index <- length(positive.flows) + 1
  entry <- list(end.date, final.charge, TRUE, "Final charge")
  positive.flows[[index]] <- entry
  positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  for (s in 1:months.to.end) {
    monthly.rent.payment.date <- arrival.date + months(development.months + s)
    if (is.na(monthly.rent.payment.date)) {
      monthly.rent.payment.date <- arrival.date - days(3) + months(development.months + s)
      if (is.na(monthly.rent.payment.date)) {
        print("No mames no puede ser")
      }
    }
    index <- length(positive.flows) + 1
    entry <-list(monthly.rent.payment.date, monthly.rent, TRUE, "Monthly rent payment")
    positive.flows[[index]] <- entry
    positive.flows.df <- rbind(positive.flows.df, entry, stringsAsFactors=FALSE)
  }
  names(positive.flows.df) <- c("Date","Amount","Income","Concept")
  #names(negative.flows.df) <- c("Date","Amount","Income","Concept")
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

break.vec <- c(seq(from=floor_date(as_date(Sys.time()), unit="month"),
                   to=ceiling_date(max(as_date(positive.flows.df$Date)), unit="month"),
                   by="month"))

ggplot(data=positive.flows.df, aes(x=as_date(positive.flows.df$Date), y=positive.flows.df$Amount)) +
  geom_point(color="blue", size=0.5) + 
  ylim(c(0,max(positive.flows.df$Amount)+1000)) +
  xlim(c(as_date(Sys.time()),max(as_date(positive.flows.df$Date)))) +
  #geom_hline(yintercept=mean(positive.flows$Amount), size=1, color="cyan") +
  #geom_linerange(aes(x=as_datetime(positive.flows$Date), ymax=positive.flows$Amount, ymin=0), color="#00AFBB") +
  geom_linerange(aes(x=as_date(positive.flows.df$Date), ymax=positive.flows.df$Amount, ymin=0, color=positive.flows.df$Concept)) +
  labs(title="Revenues", x="Time (years)", y="Income") +
  guides(color=guide_legend(title="Types of income")) +
  scale_x_date(breaks = break.vec, date_minor_breaks="1 day", limits=c(as_date(Sys.time()),NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Dummy data frame
a <- c(-3,-5,-12,0,3,1,34,2,76)
now <- as_datetime(Sys.time())
b <- c()
for (i in 1:length(a)) {
  b <- c(b, now + lubridate:::months.numeric(i-1))
}
c <- c("cat","dog","cow","horse","cat","cat","cat","sebastian","elephant")
df <- data.frame(a,b,c)
df[(df$a > 0 & df$c == "cat"), ]
as_datetime(df$b)
