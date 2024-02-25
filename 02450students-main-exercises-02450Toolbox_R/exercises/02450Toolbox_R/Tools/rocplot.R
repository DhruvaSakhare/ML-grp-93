rocplot <- function(p, y){
# ROCPLOT Plots the receiver operating characteristic (ROC)  curve and
# calculates the area under the curve (AUC).
# Notice: The method requires the package caTools to be installed!
#
# Usage:
#   rocplot(p, y);
#   res = rocplot(p, y);
# 
# Input: 
#   p: Estimated probability of class 1. (Between 0 and 1.)
#   y: True class indices. (Equal to 0 or 1.)
#
# Output:
#    list containing:
#   AUC: The area under the ROC curve
#   TPR: True positive rate
#   FPR: False positive rate
#
# Author: Laura Frølich, lff@imm.dtu.dk

  ## old code assuming values of p are distinct
#res <- sort(p, decreasing=FALSE, index.return=TRUE);
#val <- res$x
#ind <- res$ix
#x = y[ind];
#FNR = cumsum(x==1)/sum(x==1);
#TPR = 1-FNR;
#TNR = cumsum(x==0)/sum(x==0);
#FPR = 1-TNR;
#TPR = c(1, TPR);
#FPR = c(1, FPR);
#AUC = t(-diff(FPR)) %*% (TPR[1:(length(TPR)-1)]+TPR[2:length(TPR)])/2;
  
  ## new code that does not require values of p to be distinct
  
  
  res <- sort(p, decreasing=FALSE, index.return=TRUE);
  val <- res$x
  ind <- res$ix
  x = y[ind];
  
  N0=sum(1-x);
  N1=sum(x);
  FNR=c(rep(0, times = length(x)), 1) # false negative rate
  TNR=c(rep(0, times = length(x)), 1) # true negative rate
  N_true=x[1];
  N_false=1-x[1];
  t=1;
  for(k in 2:length(val)){
    if(val[k-1]!=val[k]){
      t=t+1;
      FNR[t]=N_true/N1;
      TNR[t]=N_false/N0;
    }
    N_true=N_true+x[k];
    N_false=N_false+(1-x[k]);            
  }
  FNR[t+1]=1;
  FNR = FNR[1:(t+1)]
  TNR[t+1]=1;
  TNR = TNR[1:(t+1)]
  TPR = 1-FNR;
  FPR = 1-TNR;
  if(require(caTools)){
  AUC = -trapz(FPR,TPR);
  } else {
    error('rocplot.R: The package caTools is required to compute the AUC (Area Under Curve).')
  }

plot(c(0, 1), c(0, 1), col='black', type='l', xlab='False positive rate (1-Specificity)', ylab='True positive rate (Sensitivity)', main='Receiver operating characteristic (ROC)', yaxt='n', xaxt='n')
ticks <- seq(from=0, to=1, by=0.1)
axis(1, at=ticks)
axis(2, at=ticks)
mtext(paste('AUC =', round(AUC, digits=3)))
lines(FPR, TPR, col='red')
grid(nx=length(ticks), ny=length(ticks), lwd=2)

res <- list()
res$AUC <- AUC
res$TPR <- TPR
res$FPR <- FPR

res
}
