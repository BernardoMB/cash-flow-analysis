library(ggplot2)
library(lubridate)
library(latex2exp)


averagePrice <- 10000
sdPrice <- 2000
ggplot(data.frame(x = c(0, 2 * averagePrice)), aes(x = x)) + 
  stat_function(fun = dlnorm, args = list(mean = log(averagePrice), sd = 1 / log(sdPrice))) +
  ggtitle("Price density function") +
  labs(x = TeX('$x$'), y = TeX('$f_{Price}(x)$')) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = averagePrice)


sop=c(1, 2, 3)
probs=c(4/6, 1/6, 1/6)
vals <- c()
for (i in 1:10000) {
  val <- sampleFromMonthsDistribution(sop = sop, probs = probs)
  vals <- c(vals, val)
}
ggplot(data.frame(vals), aes(x=vals)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), color="black", fill="white") +
  geom_vline(xintercept = mean(vals))
  
