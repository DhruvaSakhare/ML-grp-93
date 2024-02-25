# Function calculate Mutual Information between two assigment matrices
# Z1: D1 x N (# clusters times observations) 
# Z2: D2 x N (# clusters times obervations)
# Written by: Søren Føns Vind Nielsen [sfvn@dtu.dk]
# October, 2016
calcMI <- function(Z1, Z2){
  P <- Z1%*%t(Z2)
  PXY <- P/sum(P)
  PXPY <- rowSums(PXY)%*% t(colSums(PXY))
  greater_than_zero <- PXY > 0
  MI <- sum(PXY[greater_than_zero]*log(PXY[greater_than_zero]/PXPY[greater_than_zero]) )
  
  # return  
  MI
# EOF  
}