####################
# Exercise 12.1.4
####################

# Load data from previous script:
source(file.path('Scripts', 'ex12_1_3.R') )

# Package for data mining
library(arules) # install.packages("arules")
library(arulesViz) # install.packages("arulesViz")

# Helper function to transform binary matrix X and labels into a list of transactions:
mat2transactions <- function(X,labels){
  N <- dim(X)[1]
  T <- vector("list", N)  
  for (i in 1:N){
    T[[i]] <- labels[ X[i,] != 0  ]
  }
  return(T)
}

# Now convert to transactions
T <- mat2transactions(Xbin,labels)
print(T)

# Now find rules using a-priori algorithm. 
# Remember to install arules using the R package manager
rules <- apriori(T, parameter = list(supp = 0.8, conf = 1, target = "rules"))

# Summary of what happened
summary(rules)

# Print the found rules:
as(rules, "data.frame")
