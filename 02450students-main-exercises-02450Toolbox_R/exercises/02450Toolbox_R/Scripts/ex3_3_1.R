####################
# Exercise 3.3.1
####################
rm(list = ls()) # Clear work space)

source("Tools/binarize.R")
source("Tools/similarity.R")

# Image to use as query
i <- 1

# Similarity: 'SMC', 'Jaccard', 'ExtendedJaccard', 'Cosine', 'Correlation'
SimilarityMeasure <- "Jaccard"

library(R.matlab)
data <- readMat(file.path("./Data/digits.mat"))

# You can also try it on the faces dataset:
# data <- readMat(file.path("./Data/wildfaces_grayscale.mat"))

names(data)
X <- data$X
dimX <- dim(X)
N <- dimX[1]
M <- dimX[2]
Q <- matrix(X[i, ], ncol = M) # The query image
Y <- X[-i, ] # All images except the i'th

# The next function is in the setup.R, load it if you have not already
# source("setup.R")
sim <- similarity(Q, Y, SimilarityMeasure)

# Sort similarities
sort_result <- sort(sim, decreasing = TRUE, index.return = TRUE)
val <- sort_result$x
j <- sort_result$ix
nj <- length(j)

# Plot five most similar and dissimilar images
npics <- 5
ndigits <- 4
mostsim <- j[1:npics]
leastsim <- j[(nj - npics + 1):nj]

imageDim <- c(sqrt(M), sqrt(M))
dim(Q) <- imageDim # reshape Q

{
  dev.new(width = 2.5, height = 3)
  image(t(Q[nrow(Q):1, ]), main = "Query image", col = gray((0:32) / 32))
}

{
  dev.new(width = 2 * npics, height = 5)
  layout(matrix(c(1:length(mostsim), (length(mostsim) + 1):(2 * length(mostsim))), 2, length(mostsim), byrow = FALSE))
  for (d in 1:npics) {
    similarImage <- Y[mostsim[d], ]
    dim(similarImage) <- imageDim
    image(t(similarImage[nrow(similarImage):1, ]), main = paste("Similarity:", format(val[d], digits = ndigits)), col = gray((0:32) / 32))


    dissimilarImage <- Y[leastsim[d], ]
    dim(dissimilarImage) <- imageDim

    image(t(dissimilarImage[nrow(dissimilarImage):1, ]), main = paste("Similarity:", format(val[nj - d + 1], digits = ndigits)), col = gray((0:32) / 32))
  }
}
