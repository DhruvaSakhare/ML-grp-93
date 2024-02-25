####################
# Exercise 2.2.1
####################

rm(list = ls()) # Clear work space

# Load the library R.matlab to enable the function readMat,
# which allows R to read the matlab .mat format.
# Install with "install.packages("R.matlab")"
library(R.matlab)

# The row of training data that we will look at
i <- 1

# Read in the data
data <- readMat(file.path("Data", "zipdata.mat"))

# Check that the structure dat contains two matrices, testdata and traindata
names(data)

# The features, i.e. images of digits, are stored in the rows of traindata,
# except for the first column. The first column contains the class of the row,
# i.e. the digit identity
X <- data$traindata[, 2:dim(data$traindata)[2]]
y <- data$traindata[, 1]

par(mfrow = c(1, 2))

# View the i'th row of X
image(as.matrix(X[i, ]), axes = FALSE, xlab = "Pixel number", main = "Digit in vector format")

# Make ticks corresponding to pixels, i.e. values in row i
axis(1, at = (1:length(X[i, ])) / length(X[i, ]), labels = 1:length(X[i, ]))

# Extract the i'th row of X and store it as the matrix I
I <- X[i, ]

# Make the matrix I have dimensions 16 by 16
dim(I) <- c(16, 16)

# view the digit in the i'th row of X as an image. The function image rotates the matrix, so we need to rearrange the columns in the matrix I in order to get the correct view of the digit.
image(I[, ncol(I):1], col = gray((32:0) / 32), main = "Digit as an image")
