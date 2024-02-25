categoric2numeric <- function(x)
{

# CATEGORIC2NUMERIC converts data matrix with categorical columns given by
# numeric values or cells to numeric columns using one out of K coding.
#
# Usage:
#   result =categoric2numeric(x)
#
# Input:
#   x               categorical column of a data matrix 
#
# Output:
#   A list containing the following variables  
#   X                   Data matrix where categoric column has been
#                           converted to one out of K coding
#   attributeNames      new vector of the M attributes
#
# Author: Laura Frølich, lff@imm.dtu.dk

N <- length(x)
val <- unique(x);
M <- length(val);
X_tmp <- matrix(rep(0, times=N*M), nrow=N)
attributeNamestmp <- rep(NaN, times=M)

for(t in 1:M)
{
       X_tmp[,t] <- (x==val[t])
       attributeNamestmp[t] <- val[t]
}
result <- list()
result$X <- X_tmp
result$attributeNames <-attributeNamestmp 
result
}
