clusterval <- function(y, i){
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
#   Rand       Rand index.
#   Jaccard    Jaccard coefficient.
#   NMI        Normalized Mutual Information
#
# Author: Laura Frølich,  [lff@imm.dtu.dk] and Søren Føns Vind Nielsen [sfvn@dtu.dk]
# Updated 18th of October - Only includes Rand, Jaccard and NMI now

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

Rand = (f11+f00)/(N*(N-1)/2);
Jaccard = f11/(N*(N-1)/2-f00);
NMI <- (calcMI(Zy,Zi))/(sqrt(calcMI(Zy,Zy)*calcMI(Zi,Zi))  )  

res <- list()
res$Rand <- Rand
res$Jaccard <- Jaccard
res$NMI <- NMI
res
}
