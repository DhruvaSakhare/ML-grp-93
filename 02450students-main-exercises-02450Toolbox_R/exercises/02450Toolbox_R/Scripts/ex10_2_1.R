####################
# Exercise 10.2.1
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

#########################
# Hierarchical clustering
#########################

# Maximum number of clusters
Maxclust <- 4

# Compute hierarchical clustering
hc <- hclust(dist(X), method = "single")

# Compute clustering by thresholding the dendrogram
i <- cutree(hc, k = Maxclust)

##############
# Plot results
##############

# Plot dendrogram
par(mfrow=c(1,1))
plot(hc)

# Plot data
par(mfrow=c(1,1))
clusterplot(X, y, i, main = "Hierarchical")

