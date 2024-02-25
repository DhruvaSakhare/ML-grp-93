####################
# Exercise 11.3.2
####################
source(file.path("Scripts", "ex11_2_1.R"))
graphics.off()

source("setup.R")

X <- rbind(X, -10)

# Estimate optimal kernel density width by leave-one-out cross-validation
widths <- 2^(-10:10)
logP <- rep(NA, times = length(widths))
for (w in 1:length(widths)) {
  res <- gausKernelDensity(X, widths[w])
  f <- res$density
  log_f <- res$log_density
  logP[w] <- sum(log_f)
}
val <- max(logP)
ind <- which.max(logP)
width <- widths[ind]
print(paste("Optimal estimated width is", width))

# Estimate density for each observation not including
# the observation itself in the density estimate
res <- gausKernelDensity(X, width)
f <- res$density

# Sort the densities
sortres <- sort(f, index.return = TRUE)
y <- sortres$x
i <- sortres$ix

# Display the index of the lowest density data object
# The outlier should have index 1001
print(i[1])

# Plot density estimate outlier scores
barplot(y[1:20], main = "Outlier score")

