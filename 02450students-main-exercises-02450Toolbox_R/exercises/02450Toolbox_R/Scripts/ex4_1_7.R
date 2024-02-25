####################
# Exercise 4.1.7
####################
rm(list = ls()) # Clear work space

library(MASS)

# Digits to include in analysis (to include all, n = 1:10)
n <- c(1) # c(1,5,9)
n <- sort(n)

# Load the library R.matlab to enable the function readMat,
# which allows R to read the matlab .mat format.
library(R.matlab) # install.packages("R.matlab")

# The row of training data that we will look at
i <- 1

# Read in the data
data <- readMat(file.path("Data", "zipdata.mat"))

# Check that the structure data contains two matrices, testdata and traindata
names(data)

ncols <- ncol(data$traindata)

# Extract digits
X <- data$traindata[, 2:ncols]
y <- data$traindata[, 1]
classNames <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
classLabels <- classNames[y + 1]

# Remove digits that are not to be inspected
j <- match(y, n)
X <- X[!is.na(j), ]
classLabels <- classLabels[!is.na(j)]
classNames <- classNames[n + 1]
y <- y[!is.na(j)]
for (k in 0:(length(n) - 1))
{
  classlab <- n[k + 1]
  y[y == classlab] <- k
}

# Compute mean, standard deviations, and covariance matrix
mu <- colMeans(X)
s <- apply(X, 2, sd)
S <- cov(X)

# Generate 10 images with same mean and standard deviation
Xgen <- mvrnorm(n = 10, mu = mu, Sigma = diag(s))

# Plot images generated using the Normal distribution
{
par(mfrow = c(2, 3))
  for (k in 1:6) {
    I <- Xgen[k, ]
    dim(I) <- c(16, 16)
    image(I[, 16:1], main = "Digits: 1-D Normal", col = gray(32:0 / 32))
  }
}


# Generate 10 images with same mean and covariance matrix
Xgen <- mvrnorm(n = 10, mu = mu, Sigma = S)

{
  par(mfrow = c(2, 3))
  for (k in 1:6) {
    I <- Xgen[k, ]
    dim(I) <- c(16, 16)
    image(I[, 16:1], main = "Digits: Multivariate Normal", col = gray(32:0 / 32))
  }
}
