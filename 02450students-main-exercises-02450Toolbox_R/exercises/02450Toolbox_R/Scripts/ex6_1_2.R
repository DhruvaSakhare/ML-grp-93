####################
# Exercise 6.1.2
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

# Number of folds for k-fold cross-validation
K <- 10

# Create k-fold cross validation partition
set.seed(1234) # for reproducibility

CV <- list()
CV$which <- createFolds(y, k = K, list = F)

# Set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

max_depths <- 1:30

# Variable for classification error
Error_train <- matrix(rep(NA, times = K * length(max_depths)), nrow = K)
Error_test <- matrix(rep(NA, times = K * length(max_depths)), nrow = K)

for (k in 1:K) {
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))

  # Extract training and test set
  X_train <- X[(CV$which != k), ]
  y_train <- y[(CV$which != k)]
  X_test <- X[(CV$which == k), ]
  y_test <- y[(CV$which == k)]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)

  class_train <- classNames[y_train + 1]

  # construct formula to fit automatically to avoid typing in each variable name
  (fmla <- as.formula(paste("class_train ~ ", paste(attributeNames, collapse = "+"))))

  # fit classification tree
  # mytree <- rpart(fmla, data=Xdataframe_train,control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")

  # Compute classification error
  for (n in 1:length(max_depths)) { # For each pruning level
    # fit classification tree
    mytree_pruned <- rpart(fmla,
      data = X_train,
      control = rpart.control(
        minsplit = 2, minbucket = 1,
        cp = 0, maxdepth = max_depths[n]
      ),
      parms = list(split = "gini"), method = "class"
    )

    # mytree_pruned <- prune(mytree,prune[n])
    predicted_classes_train <- classNames[predict(mytree_pruned, newdata = X_train, type = "vector")]
    predicted_classes_test <- classNames[predict(mytree_pruned, newdata = X_test, type = "vector")]
    Error_train[k, n] <- sum(classNames[y_train + 1] != predicted_classes_train)
    Error_test[k, n] <- sum(classNames[y_test + 1] != predicted_classes_test)
  }
}


# Reorganize data to make it easier to plot
library(tidyr)

frame <- data.frame(
  Max.depth = max_depths, Train = colSums(Error_train) / sum(CV$TrainSize),
  Test = colSums(Error_test) / sum(CV$TestSize)
)
frame <- pivot_longer(frame, cols = 2:3, names_to = "Data", values_to = "Error")

library(ggplot2)

ggplot(frame, aes(x = Max.depth, y = Error, color = Data)) +
  geom_point() +
  geom_line() +
  labs(title = "Wine decision tree: 10-fold crossvalidation", x = "Max depth", y = "Classification Error") +
  theme(plot.title = element_text(hjust = 0.5))
