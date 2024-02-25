binarize <- function(X, Y) {
  # Construct binary versions of the matrices X and Y
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  # Updated 5/12-2017 by Quoc Tien Au
  if (dim(X)[2] == 1) {
    X <- t(X)
  }

  if (dim(Y)[2] == 1) {
    Y <- t(Y)
  }

  N1 <- dim(X)[1]
  M <- dim(X)[2]
  N2 <- dim(Y)[1]
  M <- dim(Y)[2]




  "Attributes non-binary: Forcing representation to be binary.
The median of X+Y is taken in order to solve ex3_3_1"
  med <- apply(rbind(X, Y), 2, median)
  X <- t(apply(X, 1, ">", med))
  Y <- t(apply(Y, 1, ">", med))

  binarized <- list()
  binarized$X <- X
  binarized$Y <- Y

  binarized
}
