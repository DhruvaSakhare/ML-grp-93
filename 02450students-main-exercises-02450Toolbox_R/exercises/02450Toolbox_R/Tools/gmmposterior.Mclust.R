gmmposterior.Mclust<- function(model, X){
  # Calculate the posterior densities of observations in X, the  negative log-likelihood of X, and the log of the density according to the model. These quantities are returned in a list with the names
  # post: posterior densities
  # ll: negative log-likelihood
  # logpdf: log of density
  #  
  # Author: Laura Frølich, lff@imm.dtu.dk
  
  K <- model$G
  n <- dim(X)[1]
  posts <- array(rep(NA, times=K*n), dim=c(n,K))
  for(icluster in 1:K){
reqres <- require(mixtools)
    if(reqres){
      posts[,icluster] <- dmvnorm(as.matrix(X), mu = model$parameters$mean[,icluster], sigma = model$parameters$variance$sigma[,,icluster])*model$parameters$pro[icluster]
    }else{
      stop('gmmposterior.Mclust: the package mixtools was not found, but is required to calculate the posterior densities according to a model of class Mclust')
      }
     
  }
#density(i) is \sum_j \alpha_j P(x_i| \theta_j)/ exp(maxll(i))
density = rowSums(posts);
#normalize posteriors
posts = posts/density
logpdf = log(density)
ll = sum(logpdf)
res <- list()
res$post <- posts
res$ll <- -ll
res$logpdf <- logpdf
res
}
