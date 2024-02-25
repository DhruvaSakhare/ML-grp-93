####################
# Exercise 7.4.4
####################
rm(list = ls()) # Clear work space

# Load data
source("Scripts/ex7_4_3.R")
source('setup.R')

# Package for Cross-Validation
library(caret)

# K-fold cross validation
K <- 10;

# For reproducibility
set.seed(12345678)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)
# Set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for naive Bayes classifier
Distribution <- c('mvmn', 'mvmn', 'mvmn', 'mvmn');
Prior <- 'empirical';

# Use only the first three features
# f <- 1:3
# X <- X[, f, drop=FALSE]
# Distribution <- Distribution[f, drop=FALSE]
# attributeNames <- attributeNames[f]

# Variable for classification error
Error <- rep(NA, times=K)

for(k in 1:K) { # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', K, sep=''));
  
  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Fit naive Bayes classifier to training set
  mymod <- naiveBayes(X_train, y_train, distribution=Distribution, prior=Prior)
  
  # Predict model on test data    
  predictRes <- predict.naiveBayes(Xtest=X_test, mod=mymod)
  y_test_est <- predictRes$predictedClass
  
  # Compute error rate
  err <- y_test != y_test_est
  
  # Make test cases that could not be predicted count as errors
  err[is.na(err)] <- 1
  
  # Count the number of errors
  Error[k] <- sum(err)
}

# Print the error rate
print(paste('Error rate: ', sum(Error)/sum(CV$TestSize)*100), sep='')

#######################################################################################
# Done with built in library that by default assumes suboptimal Gaussian distributions!
#######################################################################################
rm(predict.naiveBayes)
library(e1071)

# Variable for classification error
Error <- rep(NA, times=K)

for(k in 1:K) { # For each crossvalidation fold
    print(paste('Crossvalidation fold ', k, '/', K, sep=''));

    # Extract training and test set
    X_train <- X[CV$which != k, ]
    y_train <- y[CV$which != k]
    X_test <- X[CV$which == k, ]
    y_test <- y[CV$which == k]
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)
    
    # Fit naive Bayes classifier to training set
    mymod <- e1071::naiveBayes(y_train ~ . , X_train)
    
    # Predict model on test data    
    y_test_est <- predict(mymod, newdata = X_test)
    
    # Compute error rate
    err <- y_test != y_test_est
    
    # Make test cases that could not be predicted count as errors
    err[is.na(err)] <- 1
    
    # Count the number of errors
    Error[k] <- sum(err)
}

# Print the error rate
print(paste('Error rate: ', sum(Error)/sum(CV$TestSize)*100), sep='')

