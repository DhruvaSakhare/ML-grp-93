####################
# Exercise 8.2.6
####################
rm(list = ls()) # Clear work space

graphics.off()

source("setup.R")

# install.packages("torch")
library(torch)

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))
N <- as.numeric(data$N)
attributeNames <- make.names(unlist(data$attributeNames))
M <- as.numeric(data$M)

# Predict alcohol content
X <- data$X[, -11]
y <- data$X[, 11]
attributeNames <- attributeNames[-11]
M <- M - 1
colnames(X) <- attributeNames

# K-fold cross-validation
K <- 10

# For reproducibility
set.seed(12345)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier

# Number of hidden units
NHiddenUnits <- 2
max_iter <- 5000
n_replicates <- 2
Ntrain <- 1

# Define the model
model <- function() { 
  nn_sequential(
    nn_linear(M, NHiddenUnits),
    nn_tanh(),
    nn_linear(NHiddenUnits, 1),
  )
}

# Define the loss
loss_fn <- nn_mse_loss()

# Variable for classification error
Error <- rep(NA, times = K)

# Create tensor objects for Torch
X_tensor <- torch_tensor(X)
y_tensor <- torch_tensor(y)

for (k in 1:K) { # For each cross-validation fold
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))

  # Extract training and test set
  X_train <- X_tensor[CV$which != k, ]
  y_train <- y_tensor[CV$which != k]
  X_test <- X_tensor[CV$which == k, ]
  y_test <- y_tensor[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  # Fit neural network to training set
  result <- train_neural_net(model, loss_fn, as.matrix(X_train), y_train,
                             max_iter = max_iter, n_replicates = n_replicates)
  
  
  y_test_est <- as.array(result$net(X_test))

  # Compute error rate
  Error[k] <- sum((as.array(y_test) - y_test_est)^2) # Count the number of errors
}


# Print the error rate
print(paste("Mean Sum of Squares Error (MSSE): ", sum(Error) / sum(CV$TestSize), sep = ""))
# Inspect the parameters of the trained network (given for last cross-validation fold)
result$net$parameters
