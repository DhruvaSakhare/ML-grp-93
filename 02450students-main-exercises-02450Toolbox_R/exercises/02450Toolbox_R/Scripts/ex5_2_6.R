####################
# Exercise 5.2.6
####################

# Load wine data
source("Scripts/ex5_1_5.R")

X <- data.frame(X)
(fmla <- as.formula(paste("y ~ ", paste(colnames(X), collapse = "+"))))   

# Fit logistic regression model to predict the type of wine
w_est <- glm(fmla, family = binomial(link = "logit"), data = X)

# Define a new data object with the attributes given in the text
x <- data.frame(cbind(6.9, 1.09, .06, 2.1, .0061, 12, 31, .99, 3.5, .44, 12))
colnames(x) <- colnames(X)

# Evaluate the logistic regression for the new data object
p <- predict(w_est, newdata = x, type = "response")

