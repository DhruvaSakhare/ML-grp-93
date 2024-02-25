####################
# Exercise 9.1.1
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth5.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

#################################################
# Fit model using bootstrap aggregation (bagging)
#################################################

# Number of rounds of bagging
L <- 100 # Variable for model parameters

# Make a empty list
w_est <- vector("list", L)

# Weights for selecting samples in each bootstrap
weights <- rep.int(1, times = N) / c(N)
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse = "+"))))

# For each round of bagging
for (l in 1:L) {
  # Choose data objects by random sampling with replacement
  i <- sample(x=1:N, prob=weights, replace=TRUE)

  # Extract training set
  X_train <- X[i, ]
  y_train <- y[i]

  # Fit logistic regression model to training data and save result
  # suppress warnings to avoid warning that fitted probabilities were close to 0 or 1
  w_est[[l]] <- suppressWarnings(glm(fmla, family = binomial(link = "logit"), data = X_train))
}

# Evaluate the logistic regression on the training data

predict_helper <- function(model, newdata, type) {
  predict(model, newdata = newdata, type = type)
}

plist <- lapply(w_est, FUN = predict_helper, newdata = X, type = "response")
p <- matrix(unlist(plist), nrow = N, byrow = FALSE)

# Estimated value of class labels (using 0.5 as threshold) by majority voting
y_est <- rowSums(p > .5) > L / 2

# Compute error rate
ErrorRate <- sum(y != y_est) / N
print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

########################
# Plot decision boundary
########################

predictionFunction <- function(Xgrid, model, type) {
  plist <- lapply(w_est, predict_helper, newd = Xgrid, type = type)
  p <- matrix(unlist(plist), nrow = dim(Xgrid)[1], byrow = FALSE)
  rowMeans(p)
}

dbplot(X, attributeNames, predictionFunction, y = y, contourLevels = 0.5,
       contourCols = "white", model = w_est, type = "response")

