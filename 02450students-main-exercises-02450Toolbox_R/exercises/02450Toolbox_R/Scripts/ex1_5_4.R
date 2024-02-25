####################
# Exercise 1.5.4
####################

rm(list = ls()) # Clear work space

# ggplot2 for plotting (https://ggplot2.tidyverse.org/)
library(ggplot2)

# Repeat loading from exercise 1.5.3:
library("R.matlab")
data <- readMat("./Data/iris.mat")

X <- data$X
y <- data$y
C <- data$C[1]
M <- data$M[1]
N <- data$N[1]
attributeNames <- as.character(unlist(data$attributeNames))
classLabels <- as.character(unlist(data$classLabels))
classNames <- as.character(unlist(data$classNames))

# Organize the data in a single data frame.
data <- as.data.frame(X)
colnames(data) <- make.names(attributeNames)
data$Type <- classLabels

# --------------------------- Classification problem ---------------------------
# The current variables X and y represent a classification problem, in
# which a machine learning model will use the sepal and petal dimensions
# (stored in the matrix X) to predict the class (species of Iris, stored in
# the variable y). A relevant figure for this classification problem could
# for instance be one that shows how the classes are distributed based on
# two attributes in matrix X:

X_c <- X
y_c <- y
attributeNames_c <- attributeNames

# Make valid columns names to avoid bugs.
colnames(X_c) <- make.names(attributeNames_c)
colnames(y_c) <- "Type"

# Add blank plot with Sepal Length and Sepal Width.
myplot <- ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Type))

# Plot points for each sensor in separate colors
myplot <- myplot + geom_point()

# Position legend
myplot <- myplot + theme(legend.position = c(0.85, 0.83))

# View plot
myplot

# Consider, for instance, if it would be possible to make a single line in
# the plot to delineate any two groups? Can you draw a line between
# the Setosas and the Versicolors? The Versicolors and the Virginicas?

# ----------------------------- Regression problem -----------------------------
# Since the variable we wish to predict is petal length, it cannot any longer
# be in the data matrix X.
# We know that the petal length corresponds to the third column in data:

X_r <- data[-3]

# To remove entries i to j from a vector x, use x[-(i:j)].
# To remove row i from a matrix X, you can use X=X[-i,].
# Likewise, to remove rows i to j,  use 'X=X[-(i:j),]'.
# To remove columns i to j use 'X=X[,-(i:j)]'.
# Both columns and rows may be removed simultaneously.

# It is not a data matrix yet as it need all entries to be of the same type.
# Since the iris class information (which is now the last column in X_r) is a
# categorical variable, we will do a one-out-of-K encoding of the variable.
# A simple way of doing this is using library mltools (install.packages('mltools'))
# as well as data.table:
library(mltools)
library(data.table)

onehot <- as.matrix(one_hot(as.data.table(factor(X_r$Type))))
colnames(onehot) <- make.names(classNames)

# The encoded information is now a 150x3 matrix. This corresponds to 150
# observations, and 3 possible species. For each observation, the matrix
# has a row, and each row has two 0s and a single 1. The placement of the 1
# specifies which of the three Iris species the observations was.

# We need to replace the last column in X (which was the not encoded
# version of the species data) with the encoded version:
X_r <- as.matrix(cbind(X_r[-4], onehot))
attributeNames_r <- colnames(X_r)

y_r <- as.matrix(data[3])
targetName_r <- colnames(y_r)

# Now, X is of size 150x6 corresponding to the three measurements of the
# Iris that are not the petal length as well as the three variables that
# specifies whether or not a given observations is or isn't a certain type.

# Lastly, we update M, since we now have more attributes:
N <- dim(X_r)[1]
M <- dim(X_r)[2]

# A relevant figure for this regression problem could
# for instance be one that shows how the target, that is the petal length,
# changes with one of the predictors in X:

i <- 1
ggplot(data = as.data.frame(X_r), aes_string(x = attributeNames_r[i], y = y_r)) +
  geom_point() +
  ggtitle("Iris regression problem") +
  theme(plot.title = element_text(hjust = 0.5))

# Consider if you see a relationship between the predictor variable on the
# x-axis (the variable from X) and the target variable on the y-axis (the
# variable y). Could you draw a straight line through the data points for
# any of the attributes.
# Note that, when i is 3, 4, or 5, the x-axis is based on a binary
# variable, in which case a scatter plot is not as such the best option for
# visualizing the information.
