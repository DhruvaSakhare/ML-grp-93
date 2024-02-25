####################
# Exercise 8.3.3
####################
rm(list = ls()) # Clear work space

graphics.off()

source("setup.R")

# Package for Logisitic Regression with regularization
library(glmnet) # install.packages("glmnet")

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


## Fit a regularized multinomial regression model
regularization_strength <- 1e3

# Try a a high strength, e.g. 1e3 and analyze the distributions of labels
# Especially for synth2, synth3, and synth 4.
model <- glmnet(X_train, y_train, family = "multinomial", alpha = 0,
                lambda = regularization_strength)

# Predict labels for both sets for current regularization strength
y_train_est <- as.integer(predict(model, as.matrix(X_train), type = "class",
                                  s = regularization_strength))

# Get the predicted output for the test data
y_test_est <- as.integer(predict(model, as.matrix(X_test), type = "class",
                                 s = regularization_strength))

# Compute error rate
ErrorRate <- sum(y_test != y_test_est) / N_test
print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

# Plot results
breaks <- c(seq(-.5, 3.5, 1))

par(mfrow = c(1, 1))
par(cex.main = 2, cex.lab = 1, cex.axis = 1) # Define size of text
par(mar = c(5, 6, 4, 1) + .1) # Increase margin size to allow for larger axis labels

hist(y_train-1, breaks, freq = FALSE, ylim = c(0, 1))
hist(y_test-1, breaks, freq = FALSE, ylim = c(0, 1))
hist(y_test_est-1, breaks, freq = FALSE, ylim = c(0, 1))
