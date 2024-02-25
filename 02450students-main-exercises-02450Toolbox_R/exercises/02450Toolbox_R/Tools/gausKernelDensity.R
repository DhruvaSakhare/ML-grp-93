gausKernelDensity <- function(X, width){
# function to calculate efficiently leave-one-out Gaussian Kernel Density estimate
# Input: 
#   X        N x M data matrix
#   width    variance of the Gaussian kernel
#
# Output: 
#  density        vector of estimated densities
#  log_density    vector of estimated log_densities
#
# Author: Laura Frølich, lff@imm.dtu.dk

N <- dim(as.matrix(X))[1]
M <- dim(as.matrix(X))[2]

# Calculate squared euclidean distance between data points
# given by ||x_i-x_j||_F^2=||x_i||_F^2-2x_i^Tx_j+||x_i||_F^2 efficiently
x2=matrix(rowSums(X^2));
D=x2[,rep(1, times=N)]-2*(X%*%t(X))+t(x2[,rep(1, times=N)]);

# Evaluate densities to each observation
Q=exp(-1/(2*width)*D);
# do not take density generated from the data point itself into account
Q=Q-diag(diag(Q));

sQ <- rowSums(Q);
density=1/((N-1)*sqrt(2*pi*width)^M)*sQ;
log_density=-log(N-1)-M/2*log(2*pi*width)+log(sQ);

  result <- list()
  result$density <- density
  result$log_density <- log_density
result
}
