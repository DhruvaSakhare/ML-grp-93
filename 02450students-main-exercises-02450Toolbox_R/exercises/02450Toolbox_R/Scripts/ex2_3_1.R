####################
# Exercise 2.3.1
####################

rm(list = ls()) # Clear work space

library(FNN)
library(R.matlab)

# Read in the data
data <- readMat(file.path("Data", "zipdata.mat"))

# Check that the structure dat contains two matrices, testdata and traindata
names(data)

# The features, i.e. images of digits, are stored in the rows of traindata,
# except for the first column. The first column contains the class of the row,
# i.e. the digit identity
X <- data$traindata[, 2:dim(data$traindata)[2]]
y <- data$traindata[, 1]

Xtest <- data$testdat[, 2:dim(data$testdat)[2]]
ytest <- data$testdat[, 1]

# This is like in the exercise
digits_to_inspect <- 0:1

# If you want to try more digits
# digits_to_inspect = 0:9

# Find the observations that relate to the digits chosen in digits_to_inspect
inds <- !is.na(match(y, digits_to_inspect))
indstest <- !is.na(match(ytest, digits_to_inspect))

# Extract the rows of X found above
X <- X[inds, ]
y <- y[inds]
Xtest <- Xtest[indstest, ]
ytest <- ytest[indstest]


# Get the column means of X, subtract them from each row,
# and perform and SVD on the resulting matrix
means <- colMeans(X)
Xzeromean <- t(apply(X, 1, "-", means))
Xtestzeromean <- t(apply(Xtest, 1, "-", means))
svdres <- svd(Xzeromean)

# Extract the matrices containing the left and right singular vectors, respectively
U <- svdres$u
V <- svdres$v

K <- 5 # Number of principal components to use
pc_projections <- Xzeromean %*% V[, 1:K]
pc_projectionstest <- Xtestzeromean %*% V[, 1:K]

preds <- knn(pc_projections, pc_projectionstest, cl = y)

error_rate <- mean(preds != ytest)
print(error_rate)

