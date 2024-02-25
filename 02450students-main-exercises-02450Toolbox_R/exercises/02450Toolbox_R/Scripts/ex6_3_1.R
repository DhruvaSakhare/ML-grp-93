####################
# Exercise 6.3.1
####################
rm(list = ls()) # Clear work space

# If the package FNN is not already installed,
# install it using install.packages("FNN")
library(FNN)

library(caret)
library(ggplot2)
library(scales)


# Load data
library(R.matlab)
data <- readMat(file.path("Data", "synth1.mat"))
X <- data$X
N <- data$NC
M <- data$M
y <- as.factor(data$y)
C <- data$C

X_train <- data$X.train
N_train <- data$N.train
y_train <- factor(data$y.train)

X_test <- data$X.test
N_test <- data$N.test
y_test <- factor(data$y.test)

classNames <- make.names(unlist(data$classNames))
attributeNames <- make.names(unlist(data$attributeNames))

X <- data.frame(X)
colnames(X) <- attributeNames

X_train <- data.frame(X_train)
colnames(X_train) <- attributeNames

X_test <- data.frame(X_test)
colnames(X_test) <- attributeNames


# ------------------------------
# Make a scatterplot of the data
# ------------------------------

color <- c("blue", "red", "green", "magenta")
minmaxx <- c(min(X_train[, 1]), max(X_train[, 1]))

par(mfrow = c(1, 1))
plot(minmaxx, minmaxx, type = "n", main = "Synthetic data", ylab = "", xlab = "")
for (c in 1:C) {
  points(X_train[y_train == c - 1, 1],
    X_train[y_train == c - 1, 2],
    pch = 20, col = color[c]
  )
}
points(X_test[, 1], X_test[, 2], pch = 4)

# -------------------
# K-nearest neighbors
# -------------------

K <- 5 # Number of neighbors

# Use KNN classify to find the K nearest neighbors
y_test_est <- knn(X_train, X_test,
  cl = y_train, k = K,
  prob = FALSE, algorithm = "kd_tree"
)

# Plot estimated classes for test data
minmaxx <- c(min(X_test[, 1]), max(X_test[, 1]))
plot(minmaxx, minmaxx, main = "Synthetic data", ylab = "", xlab = "", type = "n")
for (c in 1:C) {
  points(X_test[y_test_est == c - 1, 1], X_test[y_test_est == c - 1, 2],
    pch = 21, col = color[c]
  )
}

# Plot confusion matrix

tr <- classNames[as.numeric(y_test)]
pr <- classNames[as.numeric(y_test_est)]

cm <- confusionMatrix(data = factor(tr), reference = factor(pr))
values <- as.numeric(cm$table)
accuracy <- cm$overall[1]
error_rate <- 1 - accuracy

TClass <- rep(classNames, each = C)
PClass <- rep(classNames, times = C)

df <- data.frame(TClass, PClass, values)

ggplot(data = df, mapping = aes(x = PClass, y = TClass)) +
  geom_tile(aes(fill = values), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", values)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_y_discrete(limits = rev) +
  labs(x = "Predicted class", y = "Actual class") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle(sprintf(
    "Confussion matrix (Accuracy: %s, Error Rate: %s)",
    label_percent()(accuracy), label_percent()(error_rate)
  ))

