correlated_ttest <- function(r, rho, alpha=0.05){
  rhat = mean(r)
  shat = sd(r)
  J = length(r)
  sigmatilde = shat * sqrt(1 / J + rho / (1 - rho))
  
  CI = c( qt( alpha/2, df=J-1) * sigmatilde + rhat, pt(1-alpha/2, df=J-1) * sigmatilde + rhat)
  p = 2*pt( -abs(rhat) / sigmatilde, df=J - 1)  # p-value
    
  rt = list("p"=p, "CI"=CI)
  return(rt)
}

jeffrey_interval <- function(y, yhat, alpha=0.05) {
  
  m = sum(y - yhat == 0)
  n = length(y)
  a = m+.5
  b = n-m + .5
  
  CI <- c(qbeta( alpha/2, a, b), qbeta(1-alpha/2, a, b))
  thetahat <- a/(a+b)
  rt = list("thetahat"=thetahat, "CI"=CI)
  return(rt)
}

mcnemar <- function(y_true, yhatA, yhatB, alpha=0.05){
  nn <- matrix(, nrow = 2, ncol = 2)  
  c1 = yhatA - y_true == 0
  c2 = yhatB - y_true == 0  
  
  nn[1,1] = sum(c1 & c2)
  nn[1,2] = sum(c1 & !c2)
  nn[2,1] = sum(!c1 & c2)
  nn[2,2] = sum(!c1 & !c2)
  
  n12 = nn[1,2]
  n21 = nn[2,1]
  n <- sum(nn)
   Etheta = (n12-n21)/n
   Q = n**2 * (n+1) * (Etheta+1) * (1-Etheta) / ( (n*(n12+n21) - (n12-n21)**2) )
   p = (Etheta + 1)/2 * (Q-1)
   q = (1-Etheta)/2 * (Q-1)

  thetaL =  
  CI <- c( 2*qbeta( alpha/2, p, q)-1, 2*qbeta(1-alpha/2, p, q)-1)
  # thetahat <-  2*p/(p+q) - 1
  
  p <- 2*pbinom(min( n12, n21), size=n12+n21, prob=0.5  )
  
  
  print(paste("Result of McNemars test using alpha=", alpha))

   print("Comparison matrix n")
   print(nn)
  if(n12+n21 <= 10){
        print(paste("Warning, n12+n21 is low: n12+n21=",(n12+n21)))
  }
  print("Approximate 1-alpha confidence interval of theta: [thetaL,thetaU] = ")
  print(CI)
  print(paste( "p-value for two-sided test A and B have same accuracy (exact binomial test): p=", p) )
  
  rt = list("thetahat"=Etheta, "CI"=CI, "p"=p)
  return(rt)  
}