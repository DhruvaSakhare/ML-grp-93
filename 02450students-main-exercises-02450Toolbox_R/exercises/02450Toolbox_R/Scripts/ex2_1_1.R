####################
# Exercise 2.1.1
####################

rm(list = ls()) # Clear work space

# Read data into R
data <- read.csv("./Data/nanonose.csv", check.names = FALSE)

# Extract class labels of observations
classLabels <- colnames(data)
classLabels <- classLabels[-(1:2)]

# Extract attributes, i.e. sensor names
attributeNames <- data[3:10, 1]

# Remove first two rows and columns and transpose data matrix
X <- t(data[-(1:2), -(1:2)])

# Check that dimensions are as they should be (90 rows and 8 columns)
N <- dim(X)[1]
M <- dim(X)[2]

# Assign the class labels as row names and the attributes as column names
rownames(X) <- classLabels
colnames(X) <- attributeNames

# Extract the class names present in data
classNames <- sort(unique(classLabels))
C <- length(classNames)

# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels)) - 1

# Clean up a bit in the variables, since we'll call this script from later scripts:
rm(data)
