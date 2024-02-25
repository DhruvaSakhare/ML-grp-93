####################
# Exercise 5.1.7
####################
source("Scripts/ex5_1_6.R") # get data and tree model

# Define a new data object with the attributes given in the text
x <- data.frame(t(c(6.9, 1.09, .06, 2.1, .0061, 12, 31, .99, 3.5, .44, 12)))
colnames(x) <- attributeNames

# Evaluate the classification tree for the new data object
predict(mytree, newdata = x)

