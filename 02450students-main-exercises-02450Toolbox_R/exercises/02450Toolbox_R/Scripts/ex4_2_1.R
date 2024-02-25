####################
# Exercise 4.2.1
####################
rm(list = ls()) # Clear work space

# Read data into R
data <- read.csv("./Data/iris.csv")

# Inspect the contents of the variable "data".
names(data)
head(data)

# Extract the rows and columns corresponding to the data, i.e. the attribute values
X <- data[, 1:4]

# Extract attribute names from the first row
attributeNames <- colnames(data)[1:(dim(data)[2] - 1)]

# Extract unique class names from the last column
classLabels <- data[, 5]
classNames <- unique(classLabels)

# Extract class labels that match the class names
y <- match(classLabels, classNames) - 1

# Get the number of data objects, attributes, and classes
N <- dim(X)[1]
M <- dim(X)[2]
C <- length(classNames)

