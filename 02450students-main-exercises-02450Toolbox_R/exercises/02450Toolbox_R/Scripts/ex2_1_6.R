####################
# Exercise 2.1.6
####################
source("Scripts/ex2_1_1.R")

# Try this *later* (for last), and explain the effect X_s = X
# Make a to be "scaled" version of X
# X_s[, 3] = 100*X_s[, 3] # Scale/multiply attribute C with a factor 100
# Use X_s instead of X to in the script below to see the difference.
# Does it affect the two columns in the big plot equally?

stds <- apply(X, 2, sd)
par(mfrow = c(1, 1))
barplot(stds, ylab = "NanoNose: attribute standard deviation")

# -------------------------------------------
# Investigate how standardization affects PCA
# -------------------------------------------

# Subtract the column means form columns of X
Y1 <- t(apply(X, 1, "-", colMeans(X)))

# Subtract the column means form columns of X
Y2 <- t(apply(X, 1, "-", colMeans(X)))

# Devide by the standard deviation in Y2
Y2 <- t(apply(Y2, 1, "*", 1 / stds))

# Check that the standard deviation is now one for all attributes by e.g.:
# apply(Y2, 2, sd)
i <- 1
j <- 2
threshold <- 0.9

titles <- list("Zero-mean", "Zero-mean and unit variance")

for (k in 1:2) {
  
  # PCA by computing SVD of either Y1 or Y2
  if (k == 1) {
    s <- svd(Y1)
  } else {
    s <- svd(Y2)
    s$u <- -s$u # we flip U and V to obtain comparable visualizations
    s$v <- -s$v # and it doesn't impact the PCA.
  }
  
  rho <- s$d^2 / sum(s$d^2)
  U <- s$u
  S <- diag(s$d)
  V <- s$v
  Z <- U %*% S

  # Plot projection of data onto chosen principal components
  dev.new()
  par(mfcol = c(1, 1), pty = "s")
  plot(c(min(Z[, i]), max(Z[, i])), c(min(Z[, j]), max(Z[, j])),
    xlab = paste("PC", toString(i)), ylab = paste("PC", toString(j)),
    main = paste(titles[k], "\nProjection"), type = "n"
  )

  # plot points for each sensor in separate colors
  cols <- colorRamps::matlab.like2(C)
  for (c in 0:C - 1) {
    points(Z[y == c, i], Z[y == c, j], pch = 19, cex = 2)
  }
  legend("bottomright", legend = classNames, fill = cols)

  # Plot coefficients in the PC-space
  dev.new()
  par(mfcol = c(1, 1), pty = "s")
  plot(c(-1, 1), c(-1, 1),
    xlab = paste("PC", toString(i)), ylab = paste("PC", toString(j)),
    type = "n",
    main = paste(titles[k], "\nAttribute coefficients")
  )
  arrows(integer(M), integer(M),
    V[, i], V[, j],
    length = .1,
    col = "blue"
  )
  text(V[, i] * 1.1, V[, j] * 1.1, attributeNames, cex = 1.5)
  # Add a unit circle
  th <- seq(0, 2.1 * pi, 0.1)
  lines(cos(th), sin(th))

  # Plot the variance explained
  xlimits <- c(1, M)
  dev.new()
  par(mfcol = c(1, 1), pty = "s")
  plot(cumsum(rho),
    type = "o",
    xlab = "Principal components",
    ylab = "Cum. var. explained",
    xlim = xlimits,
    ylim = c(.65, 1.02),
    col = "blue",
    main = paste(titles[k], "\nVariance explanied")
  )
  grid()
  lines(xlimits, c(threshold, threshold), lty = "dashed")
}
