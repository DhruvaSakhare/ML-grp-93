similarity <- function(X, Y, method){
# SIMILARITY Computes similarity matrices
#
# Usage:
#   sim = similarity(X, Y, method)
#
# Input:
#   X       N1 x M matrix
#   Y       N2 x M matrix 
#   method  string defining one of the following similarity measure
#           'SMC', 'smc'             : Simple Matching Coefficient
#           'Jaccard', 'jac'         : Jaccard coefficient 
#           'ExtendedJaccard', 'ext' : The Extended Jaccard coefficient
#           'Cosine', 'cos'          : Cosine Similarity
#           'Correlation', 'cor'     : Correlation coefficient
#
# Output:
#   sim     Estimated similarity matrix between X and Y
#           If input is not binary, SMC and Jaccard will make each
#           attribute binary according to x>median(x)
#
# Author: Laura Frølich, lff@imm.dtu.dk
# Updated 5/12-2017 by Quoc Tien Au 

if(dim(X)[2]==1)
  {
  X = t(X)
}

if(dim(Y)[2]==1)
  {
  Y = t(Y)
}

N1 = dim(X)[1]; M = dim(X)[2];
N2 = dim(Y)[1]; M = dim(Y)[2];


switch(tolower(substr(method, 1,3)),
    'smc'= {# SMC
              if(!is.logical(X) || !is.logical(Y))
          {
        binarized <- binarize(X,Y)
        X <- binarized$X
        Y <- binarized$Y
          }

        sim = ((X%*%t(Y))+((1-X)%*%t((1-Y))))/M;},
    'jac'= { # Jaccard
        if(!is.logical(X) || !is.logical(Y))
          {
        binarized <- binarize(X,Y)
        X <- binarized$X
        Y <- binarized$Y
        }
        divide = function(x,y) {if(y==0) 0 else x/y}
        sim = mapply(divide,(X%*%t(Y)),(M-(1-X)%*%t(1-Y)));},
    'ext'= { # Extended Jaccard
        XYt = X%*%t(Y); 
        sim = XYt/(log(t(exp(sum(t(X)^2)))*exp(colSums(t(Y)^2)))-XYt);},
    'cos'={ # Cosine
         sim = (X%*%t(Y))/as.numeric(sqrt(t(sum(t(X)^2)))%*%sqrt(colSums(t(Y)^2)));},
    'cor'={ # Correlation
        if(dim(X)[1]==1)
          {
        X_= X-mean(X)
        X_ = t(apply(X_,1,'/', sd(as.vector(X_))));
      } else
          {
        X_ = t(apply(as.matrix(X),1,'-', colMeans(as.matrix(X))));
        X_ = t(apply(X_,1,'/', apply(X_,2,sd)));
          }

        if(dim(Y)[1]==1)
          {
            Y_ <- Y-mean(Y)
        Y_ = t(apply(Y_,1,'/', sd(as.vector(Y_))));
      } else
          {
            Y_ = t(apply(as.matrix(Y),1,'-', colMeans(as.matrix(Y))));
        Y_ = t(apply(Y_,1,'/', apply(Y_,2,sd)));
          }
        sim = 1/(M-1)*X_%*%t(Y_); }
)

as.vector(sim)
}
