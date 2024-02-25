####################
# Exercise 9.1.2
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth6.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

#####################################
# Fit model using boosting (AdaBoost)
#####################################

# Number of rounds of boosting
L <- 100

# Allocate variable for model parameters
w_est <- vector("list", L)

# Allocate variable for model importance weights
alpha <- rep(NA, times = L)

# Weights for selecting samples in each round of boosting
weights <- rep(1, times = N) / c(N)

#####################
# Boosting (AdaBoost)
#####################

# For each round of boosting
l <- 1
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse = "+"))))

while (l <= L) {
  # Choose data objects by random sampling with replacement
  i <- sample(x=1:N, prob=weights, replace=TRUE)

  # Extract training set
  X_train <- X[i, ]
  y_train <- y[i]

  # Fit logistic regression model to training data and save result
  w_est[[l]] <- suppressWarnings(glm(fmla, family = binomial(link = "logit"), data = X_train))

  # Make predictions on the whole data set
  p <- predict(w_est[[l]], newdata = X, type = "response")
  y_est <- p > 0.5

  # Compute error rate
  ErrorRate <- sum(weights * (y != y_est))

  # Compute model importance weight
  alpha[l] <- .5 * log((1 - ErrorRate) / ErrorRate)

  # Update weights
  weights[y == y_est] <- weights[y == y_est] * exp(-alpha[l])
  weights[y != y_est] <- weights[y != y_est] * exp(alpha[l])
  weights <- weights / sum(weights)

  # Next round
  l <- l + 1
}

# Normalize the importance weights
alpha <- alpha / sum(alpha)

# Evaluate the logistic regression on the training data

predict_helper <- function(model, newdata, type) {
  predict(model, newdata = newdata, type = type)
}

plist <- lapply(w_est, FUN = predict_helper, newdata = X, type = "response")
p <- matrix(unlist(plist), nrow = N, byrow = FALSE) # N is number of observations

# The predictions is weighted according to alpha.
y_est <- rowSums(t(apply(p > 0.5, 1, function(x, weights) x * weights, weights = alpha))) > 0.5

# Compute error rate
ErrorRate <- sum(y != y_est) / N
print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

########################
# Plot decision boundary
########################

predictionFunction <- function(Xgrid, model, type, alpha) {
  plist <- lapply(w_est, predict_helper, newd = Xgrid, type = type)
  p <- matrix(unlist(plist), nrow = dim(Xgrid)[1], byrow = FALSE)
  y_est <- rowSums(t(apply(p > 0.5, 1, function(x, weights) x * weights, weights = alpha)))
  y_est
}

dbplot(X, attributeNames, predictionFunction, y = y, contourLevels = 0.5,
       contourCols = "white", model = w_est, typ = "response", alpha = alpha)
