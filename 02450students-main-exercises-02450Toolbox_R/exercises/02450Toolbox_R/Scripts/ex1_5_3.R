####################
# Exercise 1.5.3
####################

rm(list=ls()) # Clear work space

# You will need the package "R.matlab", which contains the function "readMat".
# install.packages('R.matlab').
library('R.matlab')

# Read Matlab data into R.
data <- readMat('./Data/iris.mat')

# You can see the variables loaded in data by checking:
(names(data))

# We can then extract the information simply by:
X = data$X
y = data$y
C = data$C[1]
M = data$M[1]
N = data$N[1]
attributeNames = as.character(unlist(data$attributeNames))
classLabels = as.character(unlist(data$classLabels))
classNames = as.character(unlist(data$classNames))

# Loading the Iris data from the .mat-file was quite easy, because all the work
# of putting it into the correct format was already done. This is of course 
# likely not the case for your own data, where you'll need to do something 
# similar to the two previous exercises. We will, however, sometimes in the 
# course use .mat-files in the exercises.
