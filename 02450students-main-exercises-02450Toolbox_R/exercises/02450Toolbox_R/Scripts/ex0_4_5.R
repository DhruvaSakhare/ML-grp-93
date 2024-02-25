####################
# Exercise 0.4.5
####################

# Setup two matrices
x <- 1:5
y <- seq(from = 2, to = 10, by = 2)

# Have a look at them by typing 'x' and 'y' in the console.

# There's a difference between matrix multiplication and element wise
# multiplication:

# Transposition of y
t(y)

# Element-wise multiplication
x * y

# Matrix multiplication, however,
# since x and y are vectors the inner product is returned
x %*% y

# There are various ways to make certain type of matrices.
a1 <- matrix(1:9, nrow = 3, ncol = 3) # define explicitly
a3 <- matrix(rep(0, len = 9), nrow = 3) # zeros matrix
a4 <- diag(3) # diagonal matrix/identity matrix
a5 <- matrix(rnorm(9), nrow = 3) # random matrix (normal distributed)
a6 <- a1 # copy (changing a1 wont change a6)


# It is easy to extract and/or modify selected items from matrices.
# Here is how you can index matrix elements:
m <- a1
m[1, 1] # first element
m[1, ] # first row
m[, 2] # second column
m[1:2, 2] # specific rows in specific columns


# Similarly, you can selectively assign values to matrix elements or columns:
m[3, 3] <- 10000
m[1:2, 3] <- c(100, 1000)
m[1:2, 1] <- 0

# Logical indexing can be used to change or take only elements that
# fulfill a certain constraint, e.g.
m <- a5
m[m > 0] # display values in m that are larger than 0
m[m < 0] <- 0 # set all elements that are less than 0 to 0

# Below, several examples of common matrix operations,
# most of which we will use in the following weeks.
# First, define two matrices:
m1 <- 10 * matrix(rep(1, len = 9), nrow = 3, ncol = 3)
m2 <- a5

m1 + m2 # matrix summation
m1 %*% m2 # matrix product
m1 * m2 # element-wise multiplication
m1 > m2 # element-wise comparison
m3 <- rbind(m1, m2) # combine/concatenate matrix rows, horizontally
m4 <- cbind(m1, m2) # combine/concatenate matrix columns, vertically
dim(m3) # shape of matrix
mean(m3) # mean value of all elements
colMeans(m3) # mean values of the columns, also for summing colSums
rowMeans(m3) # mean values of the rows, also for summing rowSums
t(m3) # transpose
