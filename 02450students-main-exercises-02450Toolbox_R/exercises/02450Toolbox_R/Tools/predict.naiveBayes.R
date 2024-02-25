predict.naiveBayes <- function(Xtest, mod){
  # Input:
  # Xtest: Matrix containing test data. The number of columns must equal the number of features in the training data.
  # mod: A list of class naiveBayes as fitted by the function naiveBayes.

  # Value:
  # predictedClass: the class predicted for each row of feature observations. The number of rows is equal to the number of rows in Xtest. If there were values of features in a row that were not seen during training, the value NA is given for that row.
  # prob: the probabilities of each class given the observed feature values. The number of rows is equal to the number of rows in Xtest. If there were values of features in a row that were not seen during training, the value NA is given for all predicted probabilities in that row.
  

  if(class(mod)!='naiveBayes'){
    stop('predict.naiveBayes:  the class of the model must be "naiveBayes", as returned by the function "naiveBayes".')
  }
  
  if(mod$nFeats!=dim(Xtest)[2]){
    stop('naiveBayes: the number of columns of the test data (the argument Xtest) must be the same as the number of columns used during training the entry "nFeats" in the argument mod')
  }

  nobs <- dim(Xtest)[1]
  
  if(length(mod$mvmnFeatsIndices)>0){
    badRows <- c()
    for(imvmnFeat in 1:length(mod$mvmnFeatsIndices)){
      currentFeatInd <- mod$mvmnFeatsIndices[imvmnFeat]
      possibleFeatVals <- unlist(mod$possibleFeatVals[[imvmnFeat]])
      currentFeatVals <- Xtest[,currentFeatInd]
      notSeenDuringTraining <- !(currentFeatVals %in% possibleFeatVals)
      if(any(notSeenDuringTraining)){
        badRows <- c(badRows,which(notSeenDuringTraining))
      }      
      }
    badRows <- unique(badRows)

    if(length(badRows)>0){
      Xtest <- Xtest[-badRows,,drop=FALSE]
      }
  }
  
  classProbs_nomissing <- array(rep(NA, times=mod$nClasses*dim(Xtest)[1]), dim=c(dim(Xtest)[1], mod$nClasses))
  
  for(iclass in 1:mod$nClasses){
    classProbs_nomissing[,iclass] <- getClassProb(Xtest, mod$mvmnFeatsIndices, mod$normalFeatsIndices, mod$params[[iclass]], mod$possibleFeatVals)*mod$priorDist[iclass]
  }
  mostProbableClassInds <- apply(X=classProbs_nomissing, MARGIN=1, FUN=which.max)
  predictedClass_nomissing<- mod$classLevels[mostProbableClassInds]

  predictedClass <- rep(NA, times=nobs)
  classProbs <- array(rep(NA, times=mod$nClasses*nobs), dim=c(nobs, mod$nClasses))
  goodRows <- 1:nobs

 if(length(mod$mvmnFeatsIndices)>0){
  if(length(badRows)>0){
    goodRows <- goodRows[-badRows]
    }
}

  predictedClass[goodRows] <- predictedClass_nomissing
  classProbs[goodRows,] <- classProbs_nomissing

  mod$predictedClass <- predictedClass
  mod$prob <- classProbs
  mod
  
}


getClassProb <- function(Xtest, mvmnFeatInds, normalFeatInds, classParams, possibleFeatVals){
  nFeats <- dim(Xtest)[2]
  nobs <- dim(Xtest)[1]
  featProbs <- array(rep(NA, times=nFeats*nobs), dim=c(nobs, nFeats))

  if(length(mvmnFeatInds)>0){
    for(imvmnFeat in 1:length(mvmnFeatInds)){
      currentFeatInd <- mvmnFeatInds[imvmnFeat]
      currentFeatVals <- Xtest[,currentFeatInd]
      curParams <- unlist(classParams[[currentFeatInd]])
      featProbs[,currentFeatInd] <- unlist(sapply(X=currentFeatVals, FUN=function(x, probs, possibleFeatVals){probs[x==possibleFeatVals]}, probs=curParams, possibleFeatVals=unlist(possibleFeatVals[[imvmnFeat]])))
      }
    }
  
  if(length(normalFeatInds)>0){
    for(inormalFeat in 1:length(normalFeatInds)){
      currentFeatInd <- normalFeatInds[inormalFeat]
      curParams <- unlist(classParams[[currentFeatInd]])
      featProbs[,currentFeatInd] <- dnorm(x=Xtest[,currentFeatInd], mean=curParams[1], sd=curParams[2])
      }
    }

  apply(X=featProbs, MARGIN=1, FUN=prod) # return the product of the probabilities of observing each feature value
}
