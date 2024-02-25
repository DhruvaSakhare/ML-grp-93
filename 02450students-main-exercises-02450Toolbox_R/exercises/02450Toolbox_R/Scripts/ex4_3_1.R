####################
# Exercise 4.3.1
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
attributeNames <- unlist(data$attributeNames)

# Assign attribute names as column names of the data matrix X
colnames(X) <- make.names(attributeNames)

# We start with a box plot of each attribute
par(mfrow = c(1, 1), mar = c(10, 4, 4, 2) + 0.1)
boxplot(X, main = "Wine: Boxplot", las = 2)

# From this it is clear that there are some outliers in the Alcohol
# attribute (10x10^14 is clearly not a proper value for alcohol content)
# However, it is impossible to see the distribution of the data, because
# the axis is dominated by these extreme outliers. To avoid this, we plot a
# box plot of standardized data (using the zscore function).

par(mfrow = c(1, 1), mar = c(10, 4, 4, 2) + 0.1)
boxplot(scale(X), main = "Wine: Boxplot", las = 2)

# This plot reveals that there are clearly some outliers in the Volatile
# acidity, Density, and Alcohol attributes, i.e. attribute number 2, 8,
# and 11.

# Next, we plot histograms of all attributes.
library("tidyr")
library("ggplot2")

ggplot(gather(data.frame(X)), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = "free_x") +
  labs(x = "", y = "Count")

# This confirms our belief about outliers in attributes 2, 8, and 11.
# To take a closer look at this, we next plot histograms of the
# attributes we suspect contains outliers.

m <- c(2, 8, 11)

ggplot(gather(data.frame(X[, m])), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = "free_x") +
  labs(x = "", y = "Count")

# The histograms show that there are a few very extreme values in these
# three attributes. To identify these values as outliers, we must use our
# knowledge about the data set and the attributes. Say we expect volatide
# acidity to be around 0-2 g/dm^3, density to be close to 1 g/cm^3, and
# alcohol percentage to be somewhere between 5-20 % vol. Then we can safely
# identify the following outliers, which are a factor of 10 greater than
# the largest we expect.

idxOutlier <- X[, 2] > 20 | X[, 8] > 10 | X[, 11] > 200

# Finally we will remove these from the data set
X <- X[-which(idxOutlier), ]
y <- y[-which(idxOutlier)]
N <- N - sum(idxOutlier)

# Now, we can repeat the process to see if there are any more outliers
# present in the data. We take a look at a histogram of all attributes:

ggplot(gather(data.frame(X)), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = "free_x") +
  labs(x = "", y = "Count")

# This reveals no further outliers, and we conclude that all outliers have
# been detected and removed.

       