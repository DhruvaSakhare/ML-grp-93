####################
# Exercise 2.1.4
####################
source("Scripts/ex2_1_3.R")

# Manual projecting data onto principal component.
Z <- s$u %*% diag(s$d)

# Doing it with the built-in function.
Z <- prcomp(X)$x

i <- 1
j <- 3

## Make fancy plot
library(ggplot2)
ggplot() +
  geom_point(aes(x = Z[, i], y = Z[, j], color = classLabels), size = 4, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.22), legend.title = element_blank()) +
  labs(x = colnames(Z)[i], y = colnames(Z)[j])
