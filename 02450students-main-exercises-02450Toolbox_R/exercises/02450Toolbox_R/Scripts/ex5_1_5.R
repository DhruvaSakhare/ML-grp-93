####################
# Exercise 5.1.5
####################
rm(list = ls()) # Clear work space

# Load the data
library(R.matlab)
data <- readMat(file.path("Data", "wine.mat"))

# View content of the Matlab data structure wine.mat
names(data)

# Extract variables
X <- data$X
y <- data$y
N <- data$N
M <- data$M
C <- data$C
classNames <- unlist(data$classNames)
attributeNames <-  make.names(unlist(data$attributeNames))

# Assign attribute names as column names of the data matrix X
colnames(X) <- attributeNames

# Detect outliers
idxOutlier <- X[, 2] > 20 | X[, 8] > 10 | X[, 11] > 200

# Finally we will remove these from the data set
X <- X[-which(idxOutlier), ]
y <- y[-which(idxOutlier)]
N <- N - sum(idxOutlier)

# Remove attribute 12, Quality score
X <- X[, -12]
attributeNames <- attributeNames[-12]
M <- dim(X)[2]

