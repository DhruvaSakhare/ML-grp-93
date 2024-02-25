####################
# Exercise 11.2.3
####################
source(file.path("Scripts", "ex11_2_1.R"))
graphics.off()

source("setup.R")

# install.packages("FNN")
library(FNN) # Library for Nearest Neighbor Search

# Number of neighbors
K <- 200

# x-values to evaluate the GMM
x <- seq(from = -10, to = 10, length.out = 100)

# Find the k nearest neighbors
res <- get.knnx(data = X, query = x, k = K)
i <- res$nn.index
D <- res$nn.dist

# Compute the density
density <- 1 / (rowSums(D) / K)

# Compute the average relative density
res <- get.knnx(data = X, query = X, k = K + 1)
iX <- res$nn.index
DX <- res$nn.dist
densityX <- 1 / (rowSums(DX[, 2:dim(DX)[2]]) / K)
temp <- matrix(densityX[i[, 2:dim(i)[2]]], nrow = dim(i)[1])
avg_rel_density <- density / (rowSums(temp) / K)

# Plot KNN estimate of density
plot(x, density, main = "KNN density", type = "l")

# Plot KNN estimate of density
plot(x, avg_rel_density, main = "KNN average relative density", type = "l")

