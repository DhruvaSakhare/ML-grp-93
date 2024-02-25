####################
# Exercise 10.1.5
####################
rm(list = ls()) # Clear work space

source("setup.R")

# Load data
library(R.matlab)

# data <- readMat(file.path('Data', 'digits.mat'))
data <- readMat(file.path("Data", "wildfaces_grayscale.mat"))
X <- data$X
N <- dim(X)[1]
M <- dim(X)[2]


# Image resolution and number of colors
x <- 40 # faces
y <- 40 # faces

# x = 16; #digits
# y = 16; #digits

c <- 1

####################
# K-means clustering
####################

K <- 10 # Maximum number of clusters

# Run k-means (This will take a while to run on a large data set)
res <- kmeans(X, K)
i <- res$cluster
Xc <- res$centers


##############
# Plot results
##############

L <- 5 # Number of images to plot

# Get some random image indices
j <- sample(x = 1:N, size = L)

# Plot centroids
n1 <- ceiling(sqrt(K / 2))
n2 <- ceiling(K / n1)

par(mfrow = c(n1, n2), mar = c(0, 0, 2, 0), xaxt = "n", yaxt = "n")
for (k in 1:K) {
  centroid <- Xc[k, ]
  dim(centroid) <- c(x, y)
  image(t(centroid[nrow(centroid):1, ]), main = paste("Centroid of class", k),
        col = gray((0:32) / 32))
}

# Plot random images and corresponding centroids
{
  dev.new()
  par(mfrow = c(L, 2), mar = c(0, 0, 2, 0), xaxt = "n", yaxt = "n")
  
  for (l in 1:L) {
    observation <- X[j[l], ]
    dim(observation) <- c(x, y)
    image(t(observation[nrow(observation):1, ]),
          main = paste("Example image from class", i[j[l]]), col = gray((0:32) / 32))
  
    centroid <- Xc[i[j[l]], ]
    dim(centroid) <- c(x, y)
    image(t(centroid[nrow(centroid):1, ]),
          main = paste("Centroid of class", i[j[l]]), col = gray((0:32) / 32))
  }
}
graphics.off()
