####################
# Exercise 4.1.5
####################
rm(list = ls()) # Clear work space

# load the package "gplots", which contains the function hist2d for making 2-dimensional histograms. If the package is not already installed on your computer, an error will result from the function call library(gplots). In that case, install the package using install.packages("gplots") and then run library(gplots) again. Same for the package MASS.
#library(gplots)

library(MASS)
# Number of samples
N <- 1000

# Mean
mu <- c(13, 17)

# Standard deviation of x1
s1 <- 2

# Standard deviation of x2
s2 <- 3

# Correlation between x1 and x2
corr <- 0

# Covariance matrix
S <- matrix(c(s1^2, corr * s1 * s2, corr * s1 * s2, s2^2), nrow = 2, byrow = TRUE)

# Number of bins in histogram
NBins <- 20

# Generate samples from the Normal distribution
X <- mvrnorm(N, mu = mu, Sigma = S)

# Plot scatter plot of data
{
  xrange <- mu[1] + S[1, 1] * c(-3, 3)
  yrange <- mu[2] + S[2, 2] * c(-3, 3)
  par(mfrow = c(1, 2))
  plot(xrange, yrange, type = "n", ylab = "x2", xlab = "x1",
       main = "Scatter plot of data")
  points(X[, 1], X[, 2])
  k <- kde2d(X[,1], X[,2])
  image(k, col = gray(32:0 / 32),
        main = "2-D Normal distribution", xlab = "x1", ylab = "x2")
}
