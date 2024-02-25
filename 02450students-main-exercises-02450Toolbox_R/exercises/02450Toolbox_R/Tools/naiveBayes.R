naiveBayes <- function(X, y, distribution = "mvmn", prior = "empirical") {
  # Input:
  # X: Training data, feature values. Must be numeric.
  # y: Training labels (response variable).
  # distribution: The distributions of features. Can either be a single string or a vector of strings with length equal to the number of features. Each string then gives the distribution for a feature. The allowed distributions are "mvmn" for multivariate multinomial and "normal" for the normal distribution. The default is "mvmn".
  # prior: The prior on classes. Can either be the string "uniform" (for equal priors for all classes) or "empirical" (for class priors equal to class proportions in training data). The default is "empirical".

  # Value:
  # params: A list of fitted parameters.
  # mvmnFeatsIndices: The feature indices that follow a multivariate multinomial distribution.
  # normalFeatsIndices: The feature indices that follow a normal distribution.
  # mvmnFeatsBinary: A binary vector with length equal to the number of features. It has the value TRUE at indices where the feature follows the multivariate multinomial distribution and is otherwise false.
  # normalFeatsBinary: A binary vector with length equal to the number of features. It has the value TRUE at indices where the feature follows the normal distribution and is otherwise false.
  # classLevels: The classes in training data, i.e. values present in y.
  # nClasses: The number of classes.
  # priorDist: The prior probability of each class.
  # nFeats: The number of features, i.e. the number of columns of X.
  # classSizes: The number of observations in each class.


  allowedDists <- c("mvmn", "normal")
  allowedPriors <- c("empirical", "uniform")

  nFeats <- dim(X)[2]

  # check that the argument distribution has the correct form
  if (!is.character(distribution)) {
    stop('naiveBayes: the argument "distribution" must be one of the strings "mvmn" or "normal", or a vector where each entry is one of these strings, and where the vector has length equal to the number of columns of X.')
  }

  if (length(distribution) == 1) {
    if (!(distribution %in% allowedDists)) {
      stop(paste('naiveBayes: the argument "distribution" must be either ', paste(allowedDists, collapse = " or "), sep = ""))
    }
  } else if (length(distribution) == nFeats) {
    if (!all(distribution %in% allowedDists)) {
      stop(paste('naiveBayes: each string in the argument "distribution" must be either ', paste(allowedDists, collapse = " or "), sep = ""))
    }
  } else {
    stop('naiveBayes: the argument "distribution" must have length one or length equal to the number of columns in X')
  }

  # done checking that the argument distribution has the correct form

  # check that the argument prior has the correct form
  if (is.character(prior)) {
    if (!(prior %in% allowedPriors)) {
      stop(paste('naiveBayes: the argument "prior" was of type "character" but not equal to either ', paste(allowedPriors, collapse = " or "), ", which are the only allowed types of priors."))
    }
  } else if (is.numeric(prior)) {
    if (length(prior) != nFeats) {
      stop('naiveBayes: if the argument "prior" is of type numeric, it must be a vector of length equal to the number of columns of X')
    }
    priorsum <- sum(prior)
    if ((priorsum > 1 + .Machine$double.eps) || (priorsum < 1 - .Machine$double.eps)) {
      warning('naiveBayes: the prior distribution of classes given in the argument "prior" did not sum to one. The prior distribution has been normalized to sum to one.')
      prior <- prior / sum(prior)
    }
  } else {
    stop(paste('naiveBayes: the argument "prior" must either be a numeric vector (of length equal to the number of columns of X) or a string (equal to either ', paste(allowedPriors, collapse = " or "), " )", sep = ""))
  }
  # done checking that the argument prior has the correct form


  nFeats <- dim(X)[2]
  uniqy <- sort(unique(y))
  nClasses <- length(uniqy)
  classLevels <- uniqy
  classSizes <- table(y)



  if (length(distribution) == 1) {
    distribution <- rep(distribution, times = nFeats)
  }
  normalFeats <- distribution == "normal"
  mvmnFeats <- distribution == "mvmn"
  params <- vector("list", length = nClasses)
  ########### fit paramaters
  if (any(normalFeats)) {
    for (iclass in 1:nClasses) {
      currentClassIndices <- y == classLevels[iclass]
      if (sum(currentClassIndices) < 2) {
        stop(paste("naiveBayes: to fit a feature to a normal distribution, at least two data points from each class must be available. Less than two data points were available for the class ", classLevels[iclass], sep = ""))
      }
      mu <- colMeans(X[currentClassIndices, normalFeats, drop = FALSE], na.rm = TRUE)
      sigma <- apply(X[currentClassIndices, normalFeats, drop = FALSE], 2, fun <- function(x) {
        sd(x, na.rm = TRUE)
      }) # column sd's
      zeroVar <- sigma < .Machine$double.eps
      if (any(zeroVar)) {
        stop(paste("naiveBayes: the following columns of X were found to have zero variance: ", which(zeroVar), sep = ""))
      }
      params[[iclass]][normalFeats] <- apply(rbind(mu, sigma), FUN = list, MARGIN = 2)
    }
  }

  if (any(mvmnFeats)) {
    possibleFeatVals <- apply(X = X[, mvmnFeats, drop = FALSE], MARGIN = 2, FUN = function(x) {
      list(sort(unique(x)))
    }) # find the observed values for each feature
    for (iclass in 1:nClasses) {
      currentClassIndices <- y == classLevels[iclass]
      if (sum(currentClassIndices) == 0) {
        stop(paste("naiveBayes: there are no observations of class", classLevels[iclass]))
      }
      mvmnFeatInds <- which(mvmnFeats)
      for (ifeat in 1:length(mvmnFeatInds)) {
        nFeatValObs <- featureValueObservations(observedFeatVals = X[currentClassIndices, mvmnFeatInds[ifeat], drop = FALSE], possibleFeatVals = unlist(possibleFeatVals[[ifeat]])) # times that each value of feature was observed
        featValDistribution <- (nFeatValObs + 1) / sum(nFeatValObs + 1) # avoid zero probabilities of feature values by adding one to the number of times each value was observed
        params[[iclass]][mvmnFeatInds[ifeat]] <- list(featValDistribution)
      }
    }
  }

  ########### done fitting paramaters


  ########### calculate priors
  if (prior == "uniform") {
    priorDist <- rep(1, times = nClasses) / nClasses
  } else if (prior == "empirical") {
    priorDist <- classSizes / sum(classSizes)
  }
  ########### done calculating priors
  res <- list()
  if (any(mvmnFeats)) {
    res$possibleFeatVals <- possibleFeatVals
  } else {
    res$possibleFeatVals <- NA
  }

  res$params <- params
  res$mvmnFeatsIndices <- which(mvmnFeats)
  res$normalFeatsIndices <- which(normalFeats)
  res$mvmnFeatsBinary <- mvmnFeats
  res$normalFeatsBinary <- normalFeats
  res$classLevels <- classLevels
  res$nClasses <- nClasses
  res$priorDist <- priorDist
  res$nFeats <- nFeats
  res$classSizes <- classSizes

  class(res) <- "naiveBayes"
  res
}

featureValueObservations <- function(observedFeatVals, possibleFeatVals) {
  counts <- sapply(possibleFeatVals, FUN = function(x, observedFeatVals) {
    sum(x == observedFeatVals)
  }, observedFeatVals = observedFeatVals)
  counts
}
