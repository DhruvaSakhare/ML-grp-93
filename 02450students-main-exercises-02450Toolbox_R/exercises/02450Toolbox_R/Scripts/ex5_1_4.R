####################
# Exercise 5.1.4
####################

source("Scripts/ex5_1_3.R")

# Define a new data object (a dragon) with the attributes given in the text
x <- data.frame(t(c(0, 2, 1, 2, 1, 1, 1)))
colnames(x) <- attributeNames
x[, attributeNames] <- lapply(x[, attributeNames], factor)

# Evaluate the classification tree for the new data object
predict(mytree, newdata = x)
