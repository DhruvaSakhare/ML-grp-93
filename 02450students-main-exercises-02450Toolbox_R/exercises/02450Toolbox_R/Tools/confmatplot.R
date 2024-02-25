confmatplot <- function(G, GHAT){
  # Plot confusion matrix based on the true labels in G and the predicted labels in GHAT
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  # Edited by (november 2018): Martin Jørgensen, marjor@dtu.dk
  
  cm =  table(G, GHAT)
  classes <- sort(unique(c(G, GHAT))) # classes in alphabetical order
  nclasses <- length(classes)
  
  if(dim(cm)[2]<nclasses){
    presentCols <- colnames(cm)
    matchingcols <- match(presentCols, classes)
    mismatchingcols <- (1:nclasses)[-matchingcols]
    while(length(mismatchingcols)!=0){
      icol <- mismatchingcols[1]
      ## new code
      if(icol == 1){
        cm <- cbind(rep(0, times = dim(cm)[1]), cm[,1:dim(cm)[2]])
      }else if( icol == nclasses){
        cm <- cbind(cm[,1:dim(cm)[2]],rep(0, times = dim(cm)[1]))
      }else{
        cm <- cbind(cm[,1:(icol-1)],rep(0,times = dim(cm)[1]),cm[,icol:dim(cm)[2]])
      }
      mismatchingcols <- mismatchingcols[-1]
      ##
      }
      colnames(cm) <- classes[1:dim(cm)[2]]
      presentCols <- colnames(cm)
      matchingcols <- match(presentCols, classes)
      mismatchingcols <- (1:nclasses)[-matchingcols]
    }  

  
  # This piece is redundant (?) Actual classes < nclasses (?) - Martin J.
  #if(dim(cm)[1]<nclasses){
  #  presentRows <- rownames(cm)
  #  matchingrows <- match(presentRows, classes)
  #  mismatchingrows <- (1:nclasses)[-matchingrows]
  #  while(length(mismatchingrows)!=0){
  #    irow <- mismatchingrows[1]
  #    if(irow<dim(cm)[1]){
  #      cm <- rbind(cm[1:irow,], rep(0, times=dim(cm)[2]), cm[irow:dim(cm)[1],])
  #    }else{
  #      cm <- rbind(cm[1:irow,], rep(0, times=dim(cm)[2]))
  #    }
  #    
  #    rownames(cm) <- classes[1:dim(cm)[1]]    
  #    presentRows <- rownames(cm)
  #    matchingrows <- match(presentRows, classes)
  #    mismatchingrows <- (1:nclasses)[-matchingrows]
  #  }  
  #}
  
  nclasses <- dim(cm)[1]
  classNames <- colnames(cm)
  
  image(1:nclasses, 1:nclasses, t(cm[nclasses:1,]), main='Confusion matrix', xlab='Predicted class', ylab="Actual class", xaxt="n", yaxt="n", col=heat.colors(nclasses^2)[(nclasses^2):1])
  #image(1:nclasses, 1:nclasses, cm[nclasses:1,], main='Confusion matrix', xlab='Predicted class', ylab="Actual class", xaxt="n", yaxt="n", col=heat.colors(nclasses^2))
  errorrate <- (sum(cm)-sum(diag(cm)))/sum(cm)*100 # error rate
  accuracy <- (sum(diag(cm)))/sum(cm)*100 # accuracy
  
  mtext(paste('Accuracy = ', round(accuracy, digits=3), '%, Error Rate = ', round(errorrate, digits=3), '%', sep=''))
  
  axisseq <- 1:nclasses
  axis(1, at=axisseq, labels=FALSE)
  axis(2, at=axisseq, labels=FALSE)
  
  text(par("usr")[1] - 0.15, axisseq, srt = 90, adj = 0.5, labels = classNames[length(classNames):1], xpd = TRUE) #?
  text(axisseq, par("usr")[3] - 0.15, srt = 0, adj = 0.5, labels = classNames, xpd = TRUE)
  
  for(iclass in 1:nclasses){
    for(jclass in 1:nclasses){
      text(jclass, iclass, labels=cm[length(classNames)-iclass+1,jclass])
    }
  }
}

