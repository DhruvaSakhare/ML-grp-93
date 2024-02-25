####################
# Exercise 8.3.2
####################
rm(list = ls()) # Clear work space

graphics.off()

source("setup.R")

library(nnet) # install.packages("nnet")

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

##################################
# Fit multinomial regression model
##################################

Y_train <- factor(y_train)
Y_test <- factor(y_test)

(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse = "+"))))
model <- multinom(formula = fmla, data = X_train)

## Compute results on test data
# Get the predicted output for the test data
Y_test_est <- predict(object = model, newdata = X_test, type = "probs")

# Compute the class index by finding the class with highest probability
# from the multinomial regression model
y_test_est <- apply(Y_test_est, 1, which.max)

ErrorRate <- sum(y_test != y_test_est) / N_test
print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

##############
# Plot results
##############

# Display decision boundaries
predictionFunction <- function(Xgriddf, model) {
  Y_test_est <- predict(object = model, newdata = Xgriddf, type = "probs")
  y_test_est <- max_idx(Y_test_est)
  y_test_est
}

dbplot(X_test, attributeNames, predictionFunction, y = y_test,
       contourLevels = 0.5, contourCols = "white", model = model)

