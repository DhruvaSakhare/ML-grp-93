####################
# Exercise 8.2.2
####################
rm(list = ls()) # Clear work space

graphics.off()

source('setup.R')

# install.packages("torch")
library(torch)

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "xor.mat"))
X <- data$X
N <- as.numeric(data$N)
attributeNames <- make.names(unlist(data$attributeNames))
M <- as.numeric(data$M)
y <- data$y
C <- data$C
classNames <- unlist(data$classNames)

# K-fold cross-validation
K <- 10

# For reproducibility
set.seed(1234)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits <- 1

# Variable for classification error
Error <- rep(NA, times = K)

model <- function() { 
  nn_sequential(
    nn_linear(M, NHiddenUnits),
    nn_tanh(),
    nn_linear(NHiddenUnits, 1),
    nn_sigmoid()
  )
}

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
                             max_iter = 10000, n_replicates = 3)

  print(paste("Best loss:", result$final_loss))
  
  # Predict model on test data
  y_sigmoid = result$net(X_test)
  y_test_est <- as.integer(y_sigmoid > 0.5)

  # Compute error rate
  Error[k] <- sum(as.integer(y_test) != y_test_est)
}

# Print the error rate
print(paste("Error rate: ", sum(Error) / sum(CV$TestSize) * 100, "%", sep = ""))

# Display the decision boundary (given for last cross-validation fold)
predictionFunction <- function(X_train, net) {
  X_train <- as.matrix(X_train)
  probs <- matrix(as.array(net(X_train)), nrow = sqrt(dim(X_train)[1]), byrow = FALSE)
  probs
}

dbplot(X, attributeNames, predictionFunction, y = y,
       contourLevels = 0.5, contourCols = "white", net = result$net)
