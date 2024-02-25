####################
# Exercise 5.2.4
####################

# Load wine data
source("Scripts/ex5_1_5.R")

# Fit linear regression model to predict Alcohol from all other attributes
y <- X[, 11]
Xr <- data.frame(X[, 1:10])
(fmla <- as.formula(paste("y ~ ", paste(colnames(Xr), collapse = "+"))))
w_est <- lm(fmla, data = Xr)

# Make a scatter plot of predicted versus true values of Alcohol
y_est <- w_est$fitted.values
plot(y, y_est,
  xlab = "Alcohol (true)", ylab = "Alcohol (estimated)",
  main = "Alcohol content", pch = 20
)

# Make a histogram of the residual error
hist(y - y_est, breaks = 41, main = "Residual error")
