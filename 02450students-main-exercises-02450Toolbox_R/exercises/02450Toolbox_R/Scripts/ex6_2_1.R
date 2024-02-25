####################
# Exercise 6.2.1
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "body.mat"))

X <- data$X
y <- data$y
N <- data$N
M <- data$M
attributeNames <- make.names(unlist(data$attributeNames))

# Assign attribute names as column names of the data matrix X
colnames(X) <- attributeNames
X <- data.frame(X)

# ------------------------------------
# Linear regression criterion function
# ------------------------------------

# This function takes as input a training and a test set.
#  1. It fits a linear model on the training set using lm.
#  2. It estimates the output of the test set using predict.
#  3. It computes the sum of squared errors.

funLinreg <- function(X_train, y_train, X_test, y_test) {
  X_train <- data.frame(X_train)
  X_test <- data.frame(X_test)
  
  xnam <- paste("X", 1:dim(X_train)[2], sep = "")
  colnames(X_train) <- xnam
  colnames(X_test) <- xnam
  (fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse = "+"))))
  
  mod <- lm(fmla, data = X_train)
  preds <- predict(mod, newdata = X_test)
  return(sum((y_test - preds)^2))
}

# ---------------
# Cross validation
# ---------------
# Number of folds for k-fold cross-validation
K <- 5

# Create k-fold cross validation partition
set.seed(1234) # for reproducibility

CV <- list()
CV$which <- createFolds(y, k = K, list = F)

# Set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialize variables
Features <- matrix(rep(NA, times = K * M), nrow = K)
Error_train <- matrix(rep(NA, times = K), nrow = K)
Error_test <- matrix(rep(NA, times = K), nrow = K)
Error_train_fs <- matrix(rep(NA, times = K), nrow = K)
Error_test_fs <- matrix(rep(NA, times = K), nrow = K)

# For each cross-validation fold
for (k in 1:K) {
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))

  # Extract training and test set
  X_train <- X[(CV$which != k), ]
  y_train <- y[(CV$which != k)]
  X_test <- X[(CV$which == k), ]
  y_test <- y[(CV$which == k)]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  # Use 10-fold cross validation for sequential feature selection
  fsres <- forwardSelection(funLinreg, X_train, y_train, stoppingCrit = "minCostImprovement")

  # Extract selected features from the forward selection routing
  selected.features <- fsres$featsIncluded

  # Save the selected features
  Features[k, ] <- fsres$binaryFeatsIncluded
  # Compute squared error without feature subset selection
  Error_train[k] <- funLinreg(X_train, y_train, X_train, y_train)
  Error_test[k] <- funLinreg(X_train, y_train, X_test, y_test)
  # Compute squared error with feature subset selection
  Error_train_fs[k] <- funLinreg(X_train[, selected.features], y_train, X_train[, selected.features], y_train)
  Error_test_fs[k] <- funLinreg(X_train[, selected.features], y_train, X_test[, selected.features], y_test)

  # Show variable selection history
  # mfig(sprintf('(%d) Feature selection',k));
  I <- length(fsres$costs) # Number of iterations
  
  par(mfrow = c(1, 2))
  # Plot error criterion
  plot(fsres$costs, xlab = "Iteration", ylab = "Squared error (crossvalidation)", main = "Value of error criterion")
  # Plot feature selection sequence
  bmplot(attributeNames, 1:I, fsres$binaryFeatsIncludedMatrix)
}


# Display results
print(paste("Linear regression without feature selection:"))
print(paste("- Training error:", sum(Error_train) / sum(CV$TrainSize)))
print(paste("- Test error:", sum(Error_test) / sum(CV$TestSize)))
print(paste("Linear regression with sequential feature selection:"))
print(paste("- Training error:", sum(Error_train_fs) / sum(CV$TrainSize)))
print(paste("- Test error:", sum(Error_test_fs) / sum(CV$TestSize)))

# Show the selected features
bmplot(attributeNames, 1:K, Features, xlab = "Crossvalidation fold", ylab = "", main = "Attributes selected")
