logLik.mixEM <- function(model){
  # Extract log-likelihood from a model of class mixEM.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  
nattributes <- dim(model$x)[2]
K <- length(model$mu)
nestimatedparameters <- K*(nattributes + nattributes*(nattributes+1)/2+1)-1 
#nestimatedparameters <- nattributes*K + nattributes*(nattributes+1)/2*K+K-1
# number of mean values estimated plus the number of terms in covariance matrices estimated plus the number of prior probabilities estimated minus one due to constraint that probs must add up to 1

    val <- model$loglik
    attr(val, "nobs") <- dim(model$x)[1]
    attr(val, "df") <- nestimatedparameters
    class(val) <- "logLik"
    val
}
