####################
# Exercise 11.2.1
####################
rm(list = ls())

# Number of data objects
N <- 1000
# x-values to evaluate the histogram
x <- seq(from = -10, to = 10, length.out = 50)

# Number of attributes
M <- 1
# Allocate variable for data
X <- array(rep(NA, times = N * M), dim = c(N, M))

# Mean and covariances
m <- c(1, 3, 6)
s <- c(1, .5, 2)

# For each data object
for (n in 1:N) {
  k <- sample(1:3, 1, replace = TRUE)
  X[n, 1] <- rnorm(1, mean = m[k], sd = sqrt(s[k]))
}

# Plot histogram
hist(x = X, breaks = c(min(X), x, max(X)))

