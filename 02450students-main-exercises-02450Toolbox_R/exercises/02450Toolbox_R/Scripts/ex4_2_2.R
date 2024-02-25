####################
# Exercise 4.2.2
####################

source("Scripts/ex4_2_1.R")

# ----------------
# With base R plot
# ----------------
yvals <- c()
for (m in 1:M)
{
  res <- hist(X[, m], plot = FALSE)
  yvals <- c(yvals, res$counts)
}
  
# The argument ylim ensures that all histograms are plotted on the same y-axis
{
  par(mfrow = c(2, 2))
  for (m in 1:M)
  {
    hist(X[, m], xlab = attributeNames[m], main = "Histogram of attribute values", ylim = c(min(yvals), max(yvals)))
  }
}

# ------------
# With ggplot2
# ------------
library(ggplot2)
library(tidyr)
ggplot(gather(X), aes(x = value)) + geom_histogram(bins = 10) +
  facet_wrap(~key, scales = "free_x") + labs(x = "Value", y = "Count")


