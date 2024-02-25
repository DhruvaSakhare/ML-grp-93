forwardSelection <- function(cost_function, X, y, cvK = 10, minCostImprovement = 0, nfeats = c(), stoppingCrit = "nfeats", maxIts = Inf) {
  # Description:
  # Select features to include in model using forward selection. No intercept is included by this function.

  # Arguments:
  # cost_function: The cost function to minimize. Must take as input explanatory variables from training data, dependent variable from training data, explanatory variables from test data, dependent variable from test data, in that order (X_train, y_train, X_test, y_test).
  # X: Explanatory variables, with rows corresponding to observations and each column corresponding to an explanatory variable.
  # y: Dependent variable. Must have length equal to the number of rows in X.
  # cvK: Number of folds to use in cross-validation when evaluating the cost_function
  # minCostImprovement:  The minimum improvement in the cost function required to continue adding variables to the model
  # nfeats: The number of variables that the final model should contain.
  # stoppingCrit: The stopping criterion to use, either the number of features ('nfeats') or minimal improvement in cost function ('minCostImprovement'). Possible values are thus 'nfeats' and 'minCostImprovement'.

  # Value:
  # List containing the following variables:
  # featsIncluded: Vector of column numbers included in model
  # binaryFeatsIncluded: Vector of length equal to the number of columns. Each entry is TRUE or FALSE, indicating for each column whether it was included in the model.
  # binaryFeatsIncludedMatrix: Matrix of vectors such as that described for binaryFeatsIncluded. First row of matrix contains the binary vector indicating chosen columns in the first iteration, the second row for the second iteration and so forth. The last row is equal to the vector binaryFeatsIncluded.
  # costs: The value of the cost function after each iteration (average over cross-validation folds).



  if (!is.scalar(cvK) || !is.scalar(minCostImprovement)) {
    warning("forwardSelection: the arguments cvK and minCostImprovement must be numeric, of the type vector, and have length one. One or or both arguments did not meet these requirements.")
  }

  if (stoppingCrit != "nfeats" && stoppingCrit != "minCostImprovement") {
    stop('forwardSelection: the argument stoppingCrit must be equal to one of the strings "nfeats" or "minCostImprovement".')
  }

  if (stoppingCrit == "nfeats" && length(nfeats) != 1) {
    if (length(nfeats) == 0) {
      nfeats <- dim(X)[2]
      warning('forwardSelection: the stopping criterion chosen was "nfeats" but the argument nfeats was not supplied. The argument nfeats has been set equal to the number of variables in the data matrix X.')
    }
    if (length(nfeats) > 1) {
      nfeats <- nfeats[1]
      warning("forwardSelction: the argument nfeats must be a scalar. The supplied value for nfeats had length greater than one. Only the first entry in nfeats is used.")
    }
  }

  res <- list()
  featsIncluded <- c()
  costs <- c()
  binaryFeatsIncludedMatrix <- c()
  counter <- 1
  unusedFeats <- 1:dim(X)[2]
  previousCost <- Inf

  reqres <- require(caret)
  if (!reqres) {
    stop("forwardSelection: the package caret is required to run forwardSelection.")
  }

  CV <- list()
  CV$which <- createFolds(y, k = cvK, list = F)

  keepGoing <- TRUE
  while (keepGoing && counter < maxIts && length(unusedFeats) > 0) {
    binaryFeatsIncluded <- rep(FALSE, times = dim(X)[2])
    avgCVcosts <- c()
    for (ifeat in 1:length(unusedFeats)) {
      currentFeat <- unusedFeats[ifeat]
      currentFeatset <- c(featsIncluded, currentFeat)
      cvCosts <- c()

      for (k in 1:cvK) {
        cvCosts[k] <- cost_function(X[CV$which != k, currentFeatset],
                                    y[CV$which != k],
                                    X[CV$which == k, currentFeatset],
                                    y[CV$which == k])
      }
      avgCVcosts[ifeat] <- mean(cvCosts)
    }
    currentCost <- min(avgCVcosts)

    if (stoppingCrit == "minCostImprovement") {
      if (previousCost - currentCost < minCostImprovement) {
        keepGoing <- FALSE
        binaryFeatsIncluded[featsIncluded] <- TRUE
      } else {
        bestiFeat <- which(avgCVcosts == currentCost)[1]
        bestFeat <- unusedFeats[bestiFeat]
        unusedFeats <- unusedFeats[-bestiFeat]

        featsIncluded[counter] <- bestFeat
        binaryFeatsIncluded[featsIncluded] <- TRUE
        binaryFeatsIncludedMatrix <- rbind(binaryFeatsIncludedMatrix, binaryFeatsIncluded)
        costs[counter] <- currentCost
        previousCost <- currentCost
        counter <- counter + 1
      }
    } else {
      bestiFeat <- which(avgCVcosts == currentCost)[1]
      bestFeat <- unusedFeats[bestiFeat]
      unusedFeats <- unusedFeats[-bestiFeat]

      featsIncluded[counter] <- bestFeat
      binaryFeatsIncluded[featsIncluded] <- TRUE
      binaryFeatsIncludedMatrix <- rbind(binaryFeatsIncludedMatrix, binaryFeatsIncluded)
      costs[counter] <- currentCost
      previousCost <- currentCost
      counter <- counter + 1

      if (length(featsIncluded) == nfeats) {
        keepGoing <- FALSE
      }
    }
  }

  res$featsIncluded <- featsIncluded
  res$binaryFeatsIncluded <- binaryFeatsIncluded
  res$binaryFeatsIncludedMatrix <- binaryFeatsIncludedMatrix
  res$costs <- costs
  res
}
