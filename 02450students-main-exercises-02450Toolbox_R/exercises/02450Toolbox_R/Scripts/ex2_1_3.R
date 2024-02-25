####################
# Exercise 2.1.3
####################

source("Scripts/ex2_1_1.R")

# Subtract the column means form columns of X
Y <- t(apply(X, 1, "-", colMeans(X)))

# You can check the column means using: colMeans(Y)
colMeans(Y)

# PCA by computing SVD of Y:
s <- svd(Y)
diagS <- s$d

rho <- diagS^2 / sum(diagS^2)

# PCA can also be done with the built-in function prcomp.
# Confirm that you get the same result.
rho2 <- summary(prcomp(X, center = TRUE))$importance[2,]


threshold <- 0.9

xlimits <- c(1, M)
plot(rho,
  type = "o",
  main = "Variance explained by principal componenets",
  xlab = "Principal components",
  ylab = "Variance explained",
  xlim = xlimits,
  ylim = c(0, 1),
  col = "blue"
)

lines(cumsum(rho), type = "o", col = "orange")
lines(xlimits, c(threshold, threshold), lty = "dashed")

legend("right", # Define position
  legend = c("Individual", "Cumulative", "Threshold"), # Set strings for legend
  col = c("orange", "blue", "black"), lty = c(1, 1, 2), # Match appereance of lines
  cex = 1, bg = "lightblue"
) # Setup how the box looks (cex controls size)

