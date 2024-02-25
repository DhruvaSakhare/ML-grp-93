clusterplot <- function(X, y, i, centroids=c(), ...){
# CLUSTERPLOT Plots a clustering of a data set as well as the true class
# labels. If data is more than 2-dimensional they are projected onto the
# first two principal components. Data objects are plotted as a dot with a
# circle around. The color of the dot indicates the true class, and the
# cicle indicates the cluster index.
#
# Usage:
#    clusterplot(X, y, i) where X is an N-by-M matrix of attribute
#    values, y is an N-by-1 vector of class labels, and i is an N-by-1
#    vector of cluster indices. N is the number of data objects and C is
#    the number of classes.
#
#    clusterplot(X, y, i, Xc) where Xc is an K-by-M matrix of cluster
#    centroids plots centroids as crosses.
#
#    If the number of attributes is M=2, the clustering is plotted in the
#    attribute space. If M>2, the clusteringis plotted in the plane spanned
#    by the first two principal components. 
#
# Input:
#    X         N-by-M matrix of attribute values for N data objects with M
#              attributes.
#    y         N-by-1 vector of class labels 
#    i         N-by-1 vector of cluster indices
#    Xc        K-by-M matrix of cluster centroids (optional)
#
# Author: Laura Frølich, lff@imm.dtu.dk

if(is.vector(X)){
X <- matrix(X, nrow=length(X), byrow=FALSE)
}
N <- dim(X)[1]
M <- dim(X)[2]


DoPCA = M>2;

if(DoPCA){
  xlab <- 'PC1'
  ylab <- 'PC2'
}

if(!DoPCA){
  if(is.data.frame(X)){
    xlab <- colnames(X)[1]
    ylab <- colnames(X)[2]
  }else{
    xlab <- 'Column 1'
    ylab <- 'Column 2'
  }
}

if(DoPCA){
  cmeans = colMeans(X)
  XX = sapply(X, function(z) z-mean(z));
  res <- svd(XX);
  V <- res$v;
  X <- XX%*%V[,1:2];
  if (length(centroids)!=0) {
    # if cluster centroids have been provided, project them onto the PCA space
    centroids <- sapply(1:ncol(centroids), function(i) centroids[,i]-cmeans[i])%*%V
  }  
}



# if(DoPCA){
#     mX = colMeans(X);
#     Z = sapply(t(X), function(X, meanX) X-meanX, meanX=mX);
#     res <- svd(Z)
#     S <- diag(res$d)
#     U <- res$u
#     V <- res$v
#     X = U%*%S;
#     centroids <- sapply(X=centroids,1, function(X, meanX) X-meanX, meanX=mX)%*%V
#   }

xRange = c(min(X[,1]), max(X[,1]))
xRange = xRange + c(-1, 1)*diff(range(xRange))*0.05;
yRange = c(min(X[,2]), max(X[,2]));
yRange = yRange + c(-1, 1)*diff(range(yRange))*0.05;

# Plot clustering
ui = sort(unique(i)); K = length(ui);
Ncol <- K
clustercols <- rainbow(K)
plot(xRange, yRange, type='n', xlab=xlab, ylab=ylab, ...)
for(k in 1:K){
    points(X[i==ui[k],1], X[i==ui[k],2], col=clustercols[((k-1)%%Ncol)+1], lwd=6)
  }

if(length(centroids)!=0){
# Plot cluster centroids
for(k in 1:dim(centroids)[1]){
    points(centroids[k,1], centroids[k,2], pch=4, col=clustercols[((k-1)%%Ncol)+1], cex=3, lwd=2);
  }
}

uy = sort(unique(y)); C = length(uy);
classcols <- rainbow(C+2)[3:(C+2)]
Ncol <- C
# Plot class labels
for(k in 1:C){
    points(X[y==uy[k],1], X[y==uy[k],2], col=classcols[((k-1)%%Ncol)+1], pch=20)
  }


}
