####################
# Exercise 8.2.5
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
X <- data$X
N <- as.numeric(data$N)
attributeNames <- make.names(unlist(data$attributeNames))
M <- as.numeric(data$M)
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

# Predict red cs. white wine type
# K-fold cross-validation
K <- 10
# For reproducibility
set.seed(12345)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits <- 2
max_iter <- 5000
n_replicates <- 2

# Variable for classification error
Error <- rep(NA, times = K)

# Define the model
model <- function() {
  nn_sequential(
    nn_linear(M, NHiddenUnits),
    nn_tanh(),
    nn_linear(NHiddenUnits, 1),
    nn_sigmoid()
  )
}

# Define the loss
loss_fn <- nn_bce_loss()

# For each cross-validation fold
for (k in 1:K) {
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))

  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  result <- train_neural_net(model, loss_fn, X_train, y_train,
    max_iter = max_iter, n_replicates = n_replicates
  )

  print(paste("Best loss:", result$final_loss))

  # Predict model on test data
  y_sigmoid <- result$net(X_test)
  y_test_est <- as.integer(y_sigmoid > 0.5)

  # Compute error rate
  Error[k] <- sum(as.integer(y_test) != y_test_est)
}


# Print the error rate
print(paste("Mean Sum of Squares Error (MSSE): ", sum(Error) / sum(CV$TestSize), sep = ""))
