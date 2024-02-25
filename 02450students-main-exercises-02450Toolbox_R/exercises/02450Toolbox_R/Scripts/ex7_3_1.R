####################
# Exercise 7.3.1
####################
rm(list = ls()) # Clear work space
source("setup.R")

# Package for decision tree
library(rpart)

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))
X <- data$X
N <- nrow(X)
M <- ncol(X)

attributeNames <- make.names(unlist(data$attributeNames))
X <- data.frame(X)
colnames(X) <- attributeNames

(fmla <- as.formula(paste(
  attributeNames[11], " ~ ",
  paste(attributeNames[1:10], collapse = "+")
)))

m <- 1 # Number of cross-validation repeats
K <- 10 # Folds in (repeated) cross validation
loss <- 1 # Loss type

y_true <- matrix(NA, 0, 1)
yhat <- matrix(NA, 0, 2)
r <- matrix(NA, 0, 1)

# Set the seed to make your partition reproducible
set.seed(1234)

for (dm in 1:m) {
  CV <- list()
  CV$which <- createFolds(1:N, k = K, list = F)
  
  # For each cross-validation fold
  for (k in 1:K) {
    print(paste(sprintf("Repeat %i / %i. Cross-validation fold %i / %i", dm, m, k, K)))
    
    # Extract training and test set
    X_train <- X[CV$which != k, ]
    X_test <- X[CV$which == k, ]
    
    mytree <- rpart(fmla, data = X_train, method = "anova")

    yhatA <- predict(mytree, newdata = X_train, type = "vector")
    yhatA <- as.matrix(yhatA)

    linearMod <- lm(fmla, data = X_train)
    mytree <- rpart(fmla, data = X_train, method = "anova")

    yhatA <- predict(linearMod, X_test)
    yhatB <- predict(mytree, X_test)

    dyhat <- cbind(yhatA, yhatB)

    yhat <- rbind(yhat, dyhat)
    y_test <- as.matrix(X_test[attributeNames[11]])
    y_true <- rbind(y_true, y_test)

    dr <- mean(abs(yhatA - y_test)**loss - abs(yhatB - y_test)**loss)
    r <- rbind(r, dr)
  }
}

# Perform tests using methods in setup II (correlated ttest)
alpha <- 0.05
rho <- 1 / K
res <- correlated_ttest(r, rho, alpha = alpha)

p_setupII <- res$p
print(paste("In setup II the p-value was", p_setupII))
print("and the CI was:")
CI_setupII <- res$CI
print(CI_setupII)

if (m == 1) {
  # Only perform setup I ttest in case m = 1 (otherwise it is not defined)
  # perform statistical comparison of the models
  # compute z with squared error.
  zA <- abs(y_true - yhat[, 1])**loss
  zB <- abs(y_true - yhat[, 2])**loss
  z <- zA - zB
  print("Corresponding p-values and confidence interval using methods in setup I:")
  t.test(z, alternative = "two.sided", alpha = 0.05)
}

