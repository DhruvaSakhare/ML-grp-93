####################
# Exercise 7.1.4
####################
rm(list = ls()) # Clear work space

# Load results from previous exercise.
source("Scripts/ex7_1_1.R")
source("setup.R")

alpha <- 0.05
rt <- mcnemar(y_true[, 1], yhat[, 1], yhat[, 2], alpha = alpha)
rt$CI # Confidence interval of difference theta = thetaA - thetaB
rt$p # p-value of null hypothesis thetaA = thetaB
rt$thetahat # Estimated difference in accuracy theta = thetaA - thetaB
