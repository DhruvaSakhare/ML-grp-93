####################
# Exercise 7.1.2
####################
rm(list = ls()) # Clear work space

# Load results from previous exercise.
source("Scripts/ex7_1_1.R")
source("setup.R")

alpha <- 0.05
rt <- jeffrey_interval(y_true, yhat[, 1], alpha = alpha)
thetahatA <- rt$thetahat
CI <- rt$CI

print(paste("Theta point estimate:", thetahatA))
print(paste("CI: "))
print(CI)
