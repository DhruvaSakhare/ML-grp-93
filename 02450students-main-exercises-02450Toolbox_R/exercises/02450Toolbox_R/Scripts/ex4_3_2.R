####################
# Exercise 4.3.2
####################

source("Scripts/ex4_3_1.R")
library(ggplot2) 
library(GGally) # install.packages("GGally")

# ------------
# With ggpairs
# ------------
m <- c(1, 4, 5, 6)
ggpairs(data.frame(X[, m]), mapping = aes(color = factor(y)))

# ---------------
# With base plots
# ---------------
NumAtr <- length(m)
par(mfrow = c(NumAtr, NumAtr), mar = c(1, 1, 1, 1))
for (m1 in 1:NumAtr) {
  for (m2 in 1:NumAtr) {
    plot(unlist(X[, m2]), unlist(X[, m1]),
      col = ifelse(y == 1, "red", "black")
    )
  }
}

