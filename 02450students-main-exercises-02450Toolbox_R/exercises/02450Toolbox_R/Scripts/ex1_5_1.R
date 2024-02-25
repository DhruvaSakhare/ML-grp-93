####################
# Exercise 1.5.1
####################

rm(list=ls()) # Clear work space.

# Read data into R as data frame.
data <- read.csv("./Data/iris.csv")

# Read more about read.csv by typing '?read.csv'.

# Extract attributes.
X <- data[, 1:4]
classLabels <- data[, 5]

# Extract class labels of observations.
attributeNames <- colnames(data[1:4])

# Column and row names can be extracted using the functions 
# 'colnames' and 'rownames', respectively.

# Column and row names can also be assigned with the functions.
colnames(X) <- attributeNames

# Check that dimensions are as they should be (150 rows and 4 columns).
# Parentheses around code in R will auto-print the evaluation.
(N <- dim(X)[1])
(M <- dim(X)[2])

# Extract numeric class assignments.
y <- as.numeric(as.factor(classLabels)) - 1

# Extract the class names present in data.
classNames <- unique(classLabels)

# Extract number of classes.
(C = length(classNames))
