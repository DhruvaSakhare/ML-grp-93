####################
# Exercise 5.2.1
####################
rm(list = ls()) # Clear work space

# Number of data objects
N <- 100

# Attribute values
X <- 0:(N - 1)

# Noise
epsilon <- rnorm(mean = 0, sd = 0.1, n = N)

# Model parameters
w0 <- -.5
w1 <- 0.01

# Outputs
y <- w0 + w1 * X + epsilon

# Make a scatter plot
plot(X, y, main = "Linear regression", xlab = "X", ylab = "y")
