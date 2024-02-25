####################
# Exercise 11.1.1
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth1.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

########################
# Gaussian mixture model
########################

# Number of clusters (>= 2)
K <- 4

########################################
# Fit model (using the mclust package)
########################################
library(mclust) # install.packages("mclust")

# package that can be used to fit a gaussian mixture model.
# This package does not allow random starts of the algorithm.
# It is faster than the algorithm in mixtools.

model <- Mclust(data = X, G = K)

# Get clustering
i <- model$classification

# Get cluster centers
Xc <- t(model$parameters$mean)

# Plot clustering
clusterplot(X, y, i, Xc, main = "GMM: Clustering")

########################################
# Fit model (using the mixtools package)
########################################
library(mixtools) # install.packages("mixtools") 

# Package that can be used to fit a gaussian mixture model.
# This package does allow random starts of the algorithm.

# using the mixtools package. Defaults for maxit and epsilon are 500 and 1e-8,
# respectively. Avoid extreme running times by allowing fewer iterations and
# deeming convergence earlier by setting maxit and epsilon as done here.
# The argument verb=TRUE makes the method write output from the EM algorithm at
# each iteration. The argument verb is FALSE by default.

model <- mvnormalmixEM(x = X, k = K, maxit = 100, epsilon = 1e-2, verb = TRUE)

# Get clustering
i <- max_idx(model$posterior)

# Get cluster centers
Xc <- matrix(unlist(model$mu), nrow=length(model$mu), byrow=TRUE)

# Plot clustering
clusterplot(X, y, i, Xc, main = "GMM: Clustering")


