logLik.Mclust <- function(model){
  # Extract log-likelihood from a model of class Mclust.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  
nattributes <- dim(model$parameters$mean)[1]
nestimatedparameters <- model$G*(nattributes + nattributes*(nattributes+1)/2+1)-1 
#nestimatedparameters <- nattributes*model$G + nattributes*(nattributes+1)/2*models$G+model$G-1
# number of mean values estimated plus the number of terms in covariance matrices estimated plus the number of prior probabilities estimated minus one due to constraint that probs must add up to 1

    val <- model$loglik
    attr(val, "nobs") <- model$n
    attr(val, "df") <- nestimatedparameters
    class(val) <- "logLik"
    val
}
