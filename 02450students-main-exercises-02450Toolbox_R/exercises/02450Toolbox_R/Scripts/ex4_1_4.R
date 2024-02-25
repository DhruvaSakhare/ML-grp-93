####################
# Exercise 4.1.4
####################
rm(list = ls()) # Clear work space

# Library for multivariate normal distribution
library(MASS) # install.packages("MASS")
?mvrnorm

# Number of samples
N <- 1000

# Mean
mu <- c(13, 17)

# Covariance matrix
S <- matrix(c(4, 3, 3, 9), nrow = 2, byrow = TRUE)

# Generate samples from the Normal distribution
X <- mvrnorm(N, mu = mu, Sigma = S)

# Inspect the dimensions of the matrix containing
# the generated multivariate normal vectors.
dim(X)
