expired_clusterval_not_for_use <- function(y, i){
  # CLUSTERVAL Estimate cluster validity using entroy, purity, rand index,
  # and Jaccard coefficient.
  #
  # Usage:
  #   [Entropy, Purity, Rand, Jaccard] = clusterval(y, i);
  #
  # Input:
  #    y         N-by-1 vector of class labels 
  #    i         N-by-1 vector of cluster indices
  #
  # Output:
  #   Entropy    Entropy measure.
  #   Purity     Purity measure.
  #   Rand       Rand index.
  #   Jaccard    Jaccard coefficient.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  # Updated 3rd November 2015 to not include self-pairs in Rand # and Jaccard
  
  N = length(y);
  
  d1_ <- sort(unique(y))
  d2_ <- unlist(lapply(d1_, FUN=function(X, y) which(y==X)[1], y=y))
  jy <- rep(NA, times=N)
  for(yvalidx in 1:length(d1_)){
    yval <- d1_[yvalidx]
    jy[y==yval] <- yvalidx
  }
  Zy <- array(rep(0, times=(length(d1_)*N)), dim=c(length(d1_), N))
  for(id1 in 1:length(d1_)){
    Zy[id1,jy==id1] <- 1
  }
  Ay = t(Zy)%*%Zy;
  
  d1_ <- sort(unique(i))
  d2_ <- unlist(lapply(d1_, FUN=function(X, y) which(y==X)[1], y=i))
  ji <- rep(NA, times=N)
  for(ivalidx in 1:length(d1_)){
    ival <- d1_[ivalidx]
    ji[i==ival] <- ivalidx
  }
  Zi <- array(rep(0, times=(length(d1_)*N)), dim=c(length(d1_), N))
  for(id1 in 1:length(d1_)){
    Zi[id1,ji==id1] <- 1
  }
  Ai = t(Zi)%*%Zi;
  
  
  f11 = (sum(sum(Ai*Ay))-N)/2;
  f00 = sum(sum((1-Ai)*(1-Ay)))/2;
  mZi = rowSums(cbind(rep(0, times=dim(Zi)[1]), Zi));
  P = 1/mZi*Zi%*%t(Zy);
  e = -rowSums(cbind(rep(0, times=dim(P)[1]), P*log(P+.Machine$double.eps)));
  Entropy = sum(e*mZi)/sum(mZi);
  Purity = sum(apply(P, 1, max)*mZi)/sum(mZi);
  Rand = (f11+f00)/(N*(N-1)/2);
  Jaccard = f11/(N*(N-1)/2-f00);
  
  
  res <- list()
  res$Entropy <- Entropy
  res$Purity <- Purity
  res$Rand <- Rand
  res$Jaccard <- Jaccard
  res
}
