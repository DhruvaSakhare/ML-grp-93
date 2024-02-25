####################
# Exercise 9.1.3
####################
rm(list = ls()) # Clear work space

library(randomForest) # install.packages("randomForest")

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth7.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

#################################################
# Fit model using bootstrap aggregation (bagging)
#################################################

# Number of rounds of bagging
L <- 100

# Fit classification trees using the TreeBagger function
classAssignments <- factor(classNames[y + 1])
(fmla <- as.formula(paste("classAssignments ~ ", paste(attributeNames, collapse = "+"))))

B <- randomForest(fmla, data = X, ntree = L)

# Make predictions for the whole data set. The predict function outputs a
# vector of factor levels, so y_est can be computed by comparing the returned
# factor levels to the level of the second class. This  will give
# a 0 for "Class 1" and a 1 for "Class 2".

y_est <- predict(B, X) == factor(classNames)[2]

# Compute error rate
ErrorRate <- sum(y != y_est) / N
print(paste("Error rate: ", ErrorRate * 100, "%", sep = ""))

########################
# Plot decision boundary
########################

predictionFunction <- function(Xgrid, model, classNames) {
  y_est <- predict(model, Xgrid) == factor(classNames)[2]
  y_est
}

dbplot(X, attributeNames, predictionFunction, y = y, contourLevels = 0.5,
       contourCols = "white", model = B, classNames = classNames)
