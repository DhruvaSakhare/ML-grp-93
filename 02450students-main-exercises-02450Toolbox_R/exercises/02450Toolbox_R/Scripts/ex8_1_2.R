####################
# Exercise 8.1.2
####################
rm(list = ls()) # Clear work space

# Package for Cross-Validation
library(caret)

# Package for Logisitic Regression with regularization
library(glmnet)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

# ----------------
# Cross-validation
# ----------------

# For reproducibility, if needed
set.seed(5142)

CV <- list()
CV$which <- createFolds(y, k = 20, list = F)
CV$TrainSize <- c()
CV$TestSize <- c()

# Extract the training and test set
X_train <- X[CV$which == 1, ]
# Train on 1/20 of the data, or 5 percent
y_train <- y[CV$which == 1]
X_test <- X[CV$which != 1, ]
# Test on the rest
y_test <- y[CV$which != 1]
CV$TrainSize[1] <- length(y_train)
CV$TestSize[1] <- length(y_test)

# Standardize based on training set
mu <- colMeans(X_train)
sigma <- apply(X_train, 2, sd)

X_train <- data.frame(scale(X_train, mu, sigma))
X_test <- data.frame(scale(X_test, mu, sigma))

#----------
# Fit model
#----------

# Fit logistic regression model to training data to predict the type of wine

N_lambdas <- 20
lambda_tmp <- 10^(seq(from = -8, to = 0, length = N_lambdas))

# alpha=0 gives ridge regression
# We will use glmnet to fit, which you can install using: install.packages('glmnet')
mdl <- glmnet(X_train, y_train, family = "binomial", alpha = 0,
              lambda = lambda_tmp)

train_error <- rep(NA, N_lambdas)
test_error <- rep(NA, N_lambdas)
coefficient_norm <- rep(NA, N_lambdas)
for (k in 1:N_lambdas) {
  # Predict labels for both sets for current regularization strength
  y_train_est <- predict(mdl, newx=as.matrix(X_train), type = "class",
                         s = lambda_tmp[k])
  y_test_est <- predict(mdl, newx=as.matrix(X_test), type = "class",
                        s = lambda_tmp[k])

  # Determine training and test set error
  train_error[k] <- sum(y_train_est != y_train) / length(y_train)
  test_error[k] <- sum(y_test_est != y_test) / length(y_test)

  # Determine betas and calculate norm of parameter vector
  w_est <- predict(mdl, type = "coef", s = lambda_tmp[k])[-1]
  coefficient_norm[k] <- sqrt(sum(w_est^2))
}

min_error <- min(test_error)
lambda_opt <- lambda_tmp[which.min(test_error)]

#-------------
# Plot results
#-------------

par(mfrow = c(1, 1))
par(cex.main = 1.5) # Define size of title
par(cex.lab = 1) # Define size of axis labels
par(cex.axis = 1) # Define size of axis labels

# Plot classification error

{
  plot(range(log10(lambda_tmp)), range(100 * c(test_error, train_error)),
    type = "n",
    xlab = "Log10(lambda)", ylab = "Error (%)",
    main = "Classification error"
  )
  
  lines(log10(lambda_tmp), train_error * 100, col = "red")
  lines(log10(lambda_tmp), test_error * 100, col = "blue")
  points(log10(lambda_opt), min_error * 100, col = "green", cex = 5)
  legend("topleft", c(
    paste("Training, n=", round(length(y_train), 2)),
    paste("Test, n=", round(length(y_test), 2))
  ),
  col = c("red", "blue"), lty = 1, cex = 1
  )
  grid()
}

# Plot classification error (zoomed)
{
  plot(range(-6, -1), range(0, 4),
    type = "n",
    xlab = "Log10(lambda)", ylab = "Error (%)", main = "Classification error (zoomed)"
  )
  lines(log10(lambda_tmp), train_error * 100, col = "red")
  lines(log10(lambda_tmp), test_error * 100, col = "blue")
  points(log10(lambda_opt), min_error * 100, col = "green", cex = 5)
  text(-4, 0.5,
    labels = paste("Min error test: ", round(min_error * 100, 2), " % at 1e",
                   round(log10(lambda_opt), 1)),
    cex = 1
  )
  grid()
}

# Plot regularization vector
{
  plot(range(log10(lambda_tmp)), range(coefficient_norm),
    type = "n",
    xlab = "Log10(lambda)", ylab = "Norm",
    main = "Parameter vector L2-norm"
  )
  lines(log10(lambda_tmp), coefficient_norm)
  grid()
}

