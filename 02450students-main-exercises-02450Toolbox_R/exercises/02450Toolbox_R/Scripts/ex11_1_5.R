####################
# Exercise 11.1.5
####################
rm(list = ls())

source("setup.R")

# library(mixtools) # install.packages("mixtools")
library(mclust) # install.packages("mclust")

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth1.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

########################
# Gaussian mixture model
########################

# Range of K's to try
KRange <- 2:10
T <- length(KRange)

# Allocate variables
BIC <- rep(NA, times = T)
AIC <- rep(NA, times = T)
CVE <- rep(0, times = T)

# Create cross validation partition for evaluation

# For reproducibility
set.seed(1234)

NumTestSets <- 10

CV <- list()
CV$which <- createFolds(y, k = NumTestSets, list = F) 
CV$NumTestSets <- NumTestSets
CV$TrainSize <- c()
CV$TestSize <- c()

# For each model order (number of clusters)
for (t in 1:T) {
  # Get the current K
  K <- KRange[t]

  # Display information
  writeLines(paste("Fitting model for K =", K))

  
  # If using the package mclust to fit the model
  model <- Mclust(data = X, G = K, verbose = F)
  
  # this block of code, within the "if" conditional, 
  # is only relevant if random restarts are allowed, 
  # as is the case for the package mixtools but not for the package mclust.
  if (FALSE) {
    # Fit model
    reps <- 100
    models <- vector("list", reps)
    logliks <- rep(NA, times = reps)
    startsigma <- replicate(K, diag(apply(X, 2, sd)), simplify = FALSE)
    for (irep in 1:reps) {
      err <- TRUE
      while (err) {
        randobs <- sample(x = 1:N, size = K)
        randmu <- lapply(X = randobs, function(X, data) data[X, ], data = as.matrix(X))
        emres <- mvnormalmixEM(X, maxit = 50, eps = 1e-2,
                               mu = randmu, sigma = startsigma, verb = F)
      }
      model <- emres
      models[[irep]] <- model
      logliks[irep] <- model$loglik
    }
    whichmaxloglik <- which.max(logliks)
    model <- models[[whichmaxloglik]]
  } # end of block only relevant for the package mixtools


  # Get BIC and AIC
  BIC[t] <- BIC(model)
  AIC[t] <- AIC(model)

  # For each crossvalidation fold
  for (k in 1:CV$NumTestSets) {
    # Extract the training and test set
    X_train <- X[CV$which != k, ]
    X_test <- X[CV$which == k, ]

    # Fit model to training set
    
    # If using the package mclust to fit the model
    model <- Mclust(data = X_train, G = K,  verbose = F)
    
    # this block of code, within the "if" conditional,
    # is only relevant if random restarts are allowed,
    # as is the case for the package mixtools but not for the package mclust
    if (FALSE) {
      reps <- 5
      models <- vector("list", reps)
      logliks <- rep(NA, times = reps)
      startsigma <- replicate(K, diag(apply(X_train, 2, sd)), simplify = FALSE)
      for (irep in 1:reps) {
        err <- FALSE
        while (!err) {
          randobs <- sample(x = 1:dim(X_train)[1], size = K)
          randmu <- lapply(X = randobs, function(X, data) data[X, ], data = as.matrix(X_train))
          emres <- mvnormalmixEM(X_train, maxit = 50, eps = 1e-2,
                                 mu = randmu, sigma = startsigma)
        }
        model <- emres
        models[[irep]] <- model
        logliks[irep] <- model$loglik
      }
      whichmaxloglik <- which.max(logliks)
      model <- models[[whichmaxloglik]]
    }

    # Evaluation cross-validation error
    res <- gmmposterior(model, X_test)
    NLOGL <- res$ll
    CVE[t] <- CVE[t] + NLOGL
  }
}

##############
# Plot results
##############

library(ggplot2)
library(dplyr)
library(tidyr)

# Organize data for plotting
df <- data.frame(KRange = KRange, BIC = BIC, AIC = AIC, CVE = 2 * CVE)
df <- pivot_longer(df, c(BIC, AIC, CVE), values_to = "Value", names_to = "Type")

# Plot results
ggplot(df, aes(x = KRange, y = Value, color = Type)) +
  geom_line() + geom_point() + ylim(min(1.5*CVE), max(2*CVE))

