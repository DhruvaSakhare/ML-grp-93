####################
# Exercise 6.3.2
####################
rm(list = ls()) # Clear work space

# Package for KNN
library(FNN)

# Load data
source("Scripts/ex4_2_1.R")

# Leave-one-out cross-validation
K <- N
CV <- list()
CV$which <- createFolds(y, k = K, list = F)
CV$NumTestSets <- K

# K-nearest neighbors parameters
# Maximum number of neighbors
L <- 40

# Variable for classification error
Error <- array(rep(NA, times = K * L), dim = c(K, L))

# Build in progress bar in R
pb <- txtProgressBar(min = 0, max = K, style = 3, width = 50)

# For each cross-validation fold
print(paste("Cross-validation:"))
for (k in 1:K) {
  txtProgressBar()
  

  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  # For each number of neighbors
  for (l in 1:L) {

    # Use KNN classify to find the l-nearest neighbors
    y_test_est <- knn(X_train, X_test, cl = y_train, k = l,
                      prob = FALSE, algorithm = "kd_tree")

    # Compute number of classification errors
    Error[k, l] <- sum(y_test != y_test_est) # Count the number of errors
  }
  
  # Update progress bar
  setTxtProgressBar(pb, k)
}

## Plot the classification error rate
plot(colSums(Error) / sum(CV$TestSize) * 100, main = "Error rate",
     xlab = "Number of neighbors", ylab = "Classification error rate (%)",
     pch = 20, type = "l")
