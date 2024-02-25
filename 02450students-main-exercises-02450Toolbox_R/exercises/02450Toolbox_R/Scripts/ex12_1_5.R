####################
# Exercise 12.1.5
####################
source("setup.R")

# Package for data mining
library(arules) # install.packages("arules")

# Load wine dataset:
library(R.matlab)
data <- readMat(file.path('Data', 'wine.mat'))

# Extract variables
X <- data$X
y <- data$y
classNames <- data$classNames
attributeNames <- make.names(unlist(data$attributeNames))

# We will now transform the wine dataset into a binary format.
print("X, i.e. the wine dataset, has now been transformed into:")
v <- binarize2(X,attributeNames)
v$X
v$attributeNames
