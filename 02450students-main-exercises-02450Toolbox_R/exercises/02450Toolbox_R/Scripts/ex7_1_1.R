####################
# Exercise 7.1.1
####################
rm(list = ls()) # Clear work space

# Package for KNN
library(FNN)

# Package for Cross-Validation
library(caret)

# Load data
source("Scripts/ex4_2_1.R")

# Leave-one-out cross-validation
K <- N
CV <- list()
CV$which <- createFolds(y, k = K, list = F)

# K-nearest neighbors parameters
L <- c(1, 20, 80)
yhat <- array(rep(NA, times = N * length(L)), dim = c(N, length(L)))
y_true <- array(rep(NA, times = N * length(L)), dim = c(N, 1))

# For each cross-validation fold
for (k in 1:K) {
  print(paste("Cross-validation fold ", k, "/", K, sep = ""))

  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  y_true[k, 1] <- y_test

  # For each number of neighbors
  for (dl in 1:length(L)) {
    l <- L[dl]
    # Use KNN classify to find the l-nearest neighbors
    out <- knn(X_train, X_test,
      cl = y_train, k = l, prob = FALSE,
      algorithm = "kd_tree"
    )

    y_test_est <- as.numeric(as.character(out[1]))
    yhat[k, dl] <- y_test_est
  }
}

## Compute accuracies here using yhat and y_true

