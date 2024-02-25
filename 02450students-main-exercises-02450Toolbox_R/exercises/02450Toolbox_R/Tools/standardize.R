standardize <- function(X) {
  # Standardize the matrix X, that is, subtract mean and divide by standard deviation to yield matrix of zero mean and variance one.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  Xmean <- colMeans(X)
  Xnomean <- X - matrix(rep(Xmean, times = N), byrow = TRUE, nrow = N)

  Xsd <- apply(Xnomean, 2, sd)
  Xstandardized <- Xnomean / matrix(rep(Xsd, times = N), byrow = TRUE, nrow = N)
  Xstandardized
}
