plot.nnet <- function(model){
  # Plot a visual representation of the neural network contained in the input model.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk

  tryCatch(library(neuralnet), finally="The function plot.nnet depends on the package neuralnet, which was not found to be installed. Install this before using plot.nnet (or using plot to plot an object of class nnet).")
mynames <- c("error")
myvect <- c(model$value)

inputnames <- c("Intercept", model$coefnames)
receivinghiddennames <- paste('1layhid', 1:model$n[2], sep='')
outputtinghiddennames <- c('Intercept', paste('1layhid', 1:model$n[2], sep=''))
outputnames <- model$lev # response classes
mynams <- c()
fittedweights <- vector('list', 1)
for(ireceivinghiddenname in 1:length(receivinghiddennames)){
mynams <- c(mynams,paste(inputnames, receivinghiddennames[ireceivinghiddenname], sep='.to.'))
}
fittedweights[[1]][[1]] <- matrix(model$wts[1:(length(inputnames)*length(receivinghiddennames))], nrow=length(inputnames), byrow=FALSE)

for(ioutputname in 1:length(outputnames)){
mynams <- c(mynams,paste(outputtinghiddennames, outputnames[ioutputname], sep='.to.'))
}
fittedweights[[1]][[2]] <- matrix(model$wts[(length(inputnames)*length(receivinghiddennames)+1):length(model$wts)], nrow=length(outputtinghiddennames), byrow=FALSE)

mynames <- c(mynames, mynams)
myvect <- c(myvect, model$wts)

mymat <- matrix(myvect)
rownames(mymat) <- mynames
colnames(mymat) <- 1
nnobj <- list()
nnobj$result.matrix <- mymat
nnobj$weights <- fittedweights
nnobj$model.list <- list()
nnobj$model.list$response <- model$lev
nnobj$model.list$variables <- model$coefnames
nnobj$call <- model$call
class(nnobj) <- 'nn'
plot(x=nnobj, information=FALSE)
}
