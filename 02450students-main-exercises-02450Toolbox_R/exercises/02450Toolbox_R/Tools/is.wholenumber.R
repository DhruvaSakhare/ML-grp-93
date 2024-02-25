is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  # Check whether input is a whole number. Returns TRUE if x is a whole number, otherwise FALSE is returned.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
abs(x - round(x)) < tol
}
