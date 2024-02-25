####################
# Exercise 8.3.1
####################
rm(list = ls()) # Clear work space

graphics.off()

source("setup.R")

# install.packages("torch")
library(torch)

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

X_train <- data$X.train
N_train <- data$N.train
y_train <- data$y.train + 1

X_test <- data$X.test
N_test <- data$N.test
y_test <- data$y.test + 1

X_train <- data.frame(X_train)
colnames(X_train) <- attributeNames
X_test <- data.frame(X_test)
colnames(X_test) <- attributeNames

# Parameters for neural network classifier
NHiddenUnits <- 2 # Number of hidden units

# Define the model
model <- function() {
  nn_sequential(
    nn_linear(M, NHiddenUnits),
    nn_relu(), # 1st transfer function
    nn_linear(NHiddenUnits, C),
    nn_softmax(dim = 1) # final transfer function, normalization of logit output
  )
}

# Since we're training a multiclass problem, we cannot use binary cross entropy,
# but instead use the general cross entropy loss:
loss_fn <- nn_cross_entropy_loss()

###############################################
# Fit multiclass neural network to training set
###############################################

X_train_torch <- torch_tensor(as.matrix(X_train), dtype = torch_float32())
y_train_torch <- torch_tensor(y_train, dtype = torch_long())$squeeze()

result <- train_neural_net(model, loss_fn, X_train_torch, y_train_torch,
                           max_iter = 10000, n_replicates = 3)

##############################
# Compute results on test data
##############################

# Get the predicted output for the test data
y_test_est <- as.array(result$net(as.matrix(X_test)))

# Compute the class index by finding the class with
# highest probability for each prediction
y_test_est <- apply(y_test_est, 1, which.max)

# Compute error rate
ErrorRate <- sum(y_test != y_test_est) / N_test

print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

##############
# Plot results
##############

# Display the decision boundary
predictionFunction <- function(Xgrid, net) {
  Xgrid <- as.matrix(Xgrid)
  probs <- net(Xgrid)
  y_test_est <- apply(probs, 1, which.max)
  return (y_test_est)
}

dbplot(X, attributeNames, predictionFunction, y = y,
       contourLevels = 0.5, contourCols = "white", net = result$net)

