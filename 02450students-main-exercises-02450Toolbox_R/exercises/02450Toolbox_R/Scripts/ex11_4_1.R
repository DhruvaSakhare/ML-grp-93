####################
# Exercise 11.4.1
####################
rm(list = ls())

source("setup.R")
library(FNN)
library(sm)

# Load hand written digits data
library(R.matlab)
data <- readMat(file.path("Data", "digits.mat"))
X <- data$X
N <- data$N
M <- data$M
y <- data$y
classNames <- as.vector(unlist(data$classNames))

# Restrict the data to images of "2"
X <- X[y == 2, ]
N <- dim(X)[1]
M <- dim(X)[2]

##################################
# Gausian Kernel density estimator
##################################

# Cross-validate kernel width by leave-one-out-cross-validation
# Automatically implemented in the script gausKernelDensity
widths <- max(apply(X, 2, var)) * (2^(-10:2))

# Evaluate for a range of kernel widths
logP <- rep(NA, times = length(widths))
for (w in 1:length(widths)) {
  res <- gausKernelDensity(X, widths[w])
  density <- res$density
  log_density <- res$log_density
  logP[w] <- sum(log_density)
}
val <- max(logP)
ind <- which.max(logP)
width <- widths[ind]
print(paste("Optimal kernel width is", width))

# Evaluate density for estimated width
res <- gausKernelDensity(X, width)
density <- res$density

# Sort the densities
sortres <- sort(density, index.return = TRUE)
y <- sortres$x
i <- sortres$ix

# Plot outlier scores
par(mfrow = c(1,1))
barplot(y[1:20], main = "Gaussian Kernel Density: outlier score")

# Plot possible outliers
{
  dev.new()
  par(mfrow = c(4, 5), oma = c(0, 0, 2, 0))
  for (k in 1:20) {
    im <- X[i[k], ]
    dim(im) <- c(16, 16)
    image(t(im[16:1, ]), col = gray((32:0) / 32), main = k)
  }
  title(main = "Gaussian Kernel Density: Possible outliers", outer = TRUE)
}

######################################
# K-nearest neighbor density estimator
######################################

# Number of neighbors
K <- 5

# Find the k nearest neighbors
res <- get.knnx(data = X, query = X, k = K + 1)
idx <- res$nn.index
D <- res$nn.dist

# Compute the density
density <- 1 / (rowSums(D[, 2:dim(D)[2]]) / K)

# Sort the densities
sortres <- sort(density, index.return = TRUE)
y <- sortres$x
i <- sortres$ix

# Plot outlier scores
par(mfrow = c(1,1))
barplot(y[1:20], main = "KNN density: outlier score")

# Plot possible outliers
{
  dev.new()
  par(mfrow = c(4, 5), oma = c(0, 0, 2, 0))
  for (k in 1:20) {
    im <- X[i[k], ]
    dim(im) <- c(16, 16)
    image(t(im[16:1, ]), col = gray((32:0) / 32), main = k)
  }
  title(main = "KNN density: Possible outliers", outer = TRUE)
}

############################################
# K-nearest neigbor average relative density
############################################

# Compute the average relative density
avg_rel_density <- density / (rowSums(matrix(density[idx[, 2:dim(idx)[2]]], nrow = dim(idx)[1])) / K)

# Sort the densities
sortres <- sort(avg_rel_density, index.return = TRUE)
y <- sortres$x
i <- sortres$ix

# Plot outlier scores
par(mfrow = c(1,1))
barplot(y[1:20], main = "KNN average relative density: outlier score")

# Plot possible outliers
{
  dev.new()
  par(mfrow = c(4, 5), oma = c(0, 0, 2, 0))
  for (k in 1:20) {
    im <- X[i[k], ]
    dim(im) <- c(16, 16)
    image(t(im[16:1, ]), col = gray((32:0) / 32), main = k)
  }
  title(main = "KNN average relative density: Possible outliers", outer = TRUE)
}

#################################################
# Distance to 5'th nearest neighbor outlier score
#################################################

# Neighbor to use
K <- 5

# Find the k nearest neighbors
res <- get.knnx(data = X, query = X, k = K + 1)
i <- res$nn.index
D <- res$nn.dist

# Outlier score
f <- D[, K + 1]

# Sort the outlier scores
sortres <- sort(f, decreasing = TRUE, index.return = TRUE)
y <- sortres$x
i <- sortres$ix

# Plot kernel density estimate outlier scores
par(mfrow = c(1,1))
barplot(y[1:20], main = "Distance: Outlier score")

# Plot possible outliers
{
  dev.new()
  par(mfrow = c(4, 5), oma = c(0, 0, 2, 0))
  for (k in 1:20) {
    im <- X[i[k], ]
    dim(im) <- c(16, 16)
    image(t(im[16:1, ]), col = gray((32:0) / 32), main = k)
  }
  title(main = "Distance: Possible outliers", outer = TRUE)
}

###############################################
# Plot of random "normal" digits for comparison
###############################################

# Plot random digits (the first 20 in the data set)
{
  dev.new()
  par(mfrow = c(4, 5), oma = c(0, 0, 2, 0))
  for (k in 1:20) {
    im <- X[k, ]
    dim(im) <- c(16, 16)
    image(t(im[16:1, ]), col = gray((32:0) / 32), main = k)
  }
  title(main = "Digits", outer = TRUE)
}

