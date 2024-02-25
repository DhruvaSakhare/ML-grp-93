####################
# Exercise 6.1.1
####################
rm(list = ls()) # Clear work space

# Package for decision trees
library(rpart)

# Package for manual cross-validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))

# View content of the Matlab data structure wine2.mat
names(data)

# Extract variables
X <- data$X
y <- data$y
N <- data$N
M <- data$M
C <- data$C
classNames <- unlist(data$classNames)
attributeNames <- make.names(unlist(data$attributeNames))

# Assign attribute names as column names of the data matrix X
colnames(X) <- attributeNames
X <- data.frame(X)


# -----------------------------------------
# Create holdout cross-validation partition
# -----------------------------------------

# Set seed if you want reproducible results
set.seed(4321)

CV <- createFolds(y, k = 2)

# Add sizes of training and test data to the list CV returned by cvFolds
CV$TrainSize <- length(CV$Fold1)
CV$TestSize <- length(CV$Fold1)

max_depths <- 1:30

# Variable for classification error count
Error_train <- rep(NA, times = length(max_depths))
Error_test <- rep(NA, times = length(max_depths))

# Extract training and test set
X_train <- X[CV$Fold1, ]
y_train <- y[CV$Fold1]
X_test <- X[CV$Fold2, ]
y_test <- y[CV$Fold2]

# Fit classification tree to training set
class_train <- classNames[y_train + 1]

# Construct formula to fit automatically to avoid typing in each variable name
(fmla <- formula(paste("class_train ~ ", paste(attributeNames, collapse = "+"))))

# Compute classification error for each pruning level
for (n in 1:length(max_depths)) {
  # Fit classification tree
  mytree_pruned <- rpart(fmla,
    data = X_train,
    control = rpart.control(
      minsplit = 2, minbucket = 1,
      cp = 0, maxdepth = max_depths[n]
    ),
    parms = list(split = "gini"), method = "class"
  )

  # Calculate train and test error
  predicted_classes_train <- classNames[predict(mytree_pruned, newdata = X_train, type = "vector")]
  predicted_classes_test <- classNames[predict(mytree_pruned, newdata = X_test, type = "vector")]
  Error_train[n] <- sum(classNames[y_train + 1] != predicted_classes_train)
  Error_test[n] <- sum(classNames[y_test + 1] != predicted_classes_test)
}

# Reorganize data to make it easier to plot
library(tidyr)

frame <- data.frame(Max.depth = max_depths, Train = Error_train / CV$TrainSize,
                    Test = Error_test / CV$TestSize)
frame <- pivot_longer(frame, cols = 2:3, names_to = "Data", values_to = "Error")

library(ggplot2)

ggplot(frame, aes(x = Max.depth, y = Error, color = Data)) +
  geom_point() +
  geom_line() +
  labs(title = "Wine decision tree: Holdout crossvalidation", x = "Max depth", y = "Classification Error") +
  theme(plot.title = element_text(hjust = 0.5))

