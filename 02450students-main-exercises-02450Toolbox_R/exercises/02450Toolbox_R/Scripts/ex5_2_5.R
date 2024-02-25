####################
# Exercise 5.2.5
####################

# Load wine data
source("Scripts/ex5_1_5.R")

# Fit linear regression model to predict Alcohol from all other attributes
y <- X[, 11]
Xr <- data.frame(cbind(X[, 1:10], X[, 1]^2, X[, 2]^2, X[, 1] * X[, 2]))
xnam <- paste("X", 1:dim(Xr)[2], sep = "")
colnames(Xr) <- xnam

(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse = "+"))))
w_est <- lm(fmla, data = Xr)

# Make a scatter plot of predicted versus true values of Alcohol
y_est <- w_est$fitted.values
plot(y, y_est, xlab = "Alcohol (true)", ylab = "Alcohol (estimated)",
     main = "Alcohol content", pch = 20)

# Make a histogram of the residual error
hist(y - y_est, breaks = 41, main = "Residual error")

# Compute mean squared error
mean((y - y_est)^2)

