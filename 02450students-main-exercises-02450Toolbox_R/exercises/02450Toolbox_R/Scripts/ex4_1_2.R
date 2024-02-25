####################
# Exercise 4.1.2
####################
rm(list = ls()) # Clear work space

# Number of samples
N <- 100

# Mean
mu <- 17

# Standard deviation
s <- 2

# Number of bins in histogram
NBins <- 20

# Generate samples from the Normal distribution
X <- rnorm(N, mean = mu, sd = s)

# Plot a histogram
{
  par(mfrow = c(1, 2))
  plot(X, main = "Generated data")
  hist(X, breaks = NBins, main = "Histogram of generated data")
}

# Compute empirical mean and standard deviation
(mu_ <- mean(X))
(s_ <- sd(X))

