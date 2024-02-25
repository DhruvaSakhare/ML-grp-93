####################
# Exercise 10.1.1
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

####################
# K-means clustering
####################

# Number of clusters
K <- 4

# Run k-means
res <- kmeans(X, K)
i <- res$cluster
Xc <- res$centers

##############
# Plot results
##############

clusterplot(X, y, i, Xc, main = "K-means")
