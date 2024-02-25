####################
# Exercise 10.1.3
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth1.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(as.vector(unlist(data$attributeNames)))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

####################
# K-means clustering
####################

# Maximum number of clusters
K <- 10

# Allocate variables
Rand <- rep(NA, times = K)
Jaccard <- rep(NA, times = K)
NMI <- rep(NA, times = K)

for (k in 1:K) {
  # Run k-means
  kmeansres <- kmeans(X, k, iter.max = 100)
  i <- kmeansres$cluster
  
  # Compute cluster validities
  res <- clusterval(y, i)

  Rand[k] <- res$Rand
  Jaccard[k] <- res$Jaccard
  NMI[k] <- res$NMI
}

##############
# Plot results
##############

cols <- c("blue", "green", "red", "lightblue")
maxy <- max(c(Rand, Jaccard, NMI), na.rm = TRUE)
miny <- min(c(Rand, Jaccard, NMI), na.rm = TRUE)
plot(c(1, K), c(miny, maxy), type = "n", main = "Cluster validity", xlab = "Number of clusters", ylab = "")
lines(1:K, Rand, col = cols[1])
lines(1:K, Jaccard, col = cols[2])
lines(1:K, NMI, col = cols[3])
legend("bottomright", legend = c("Rand", "Jaccard", "NMI"), fill = cols)
