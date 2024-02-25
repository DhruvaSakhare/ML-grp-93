####################
# Exercise 7.2.1
####################
rm(list = ls()) # Clear work space

# Package for decision tree
library(rpart)

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))
X <- data$X
N <- nrow(X)
M <- ncol(X)

attributeNames <- make.names(unlist(data$attributeNames))
X <- data.frame(X)
colnames(X) <- attributeNames


## Set the seed to make your partition reproducible
set.seed(1234)
train_ind <- createDataPartition(1:N, p = 0.2, list = F)
X_train <- X[train_ind, ]
X_test <- X[-train_ind, ]

(fmla <- as.formula(paste(attributeNames[11], " ~ ",
                          paste(attributeNames[1:10], collapse = "+"))))

mytree <- rpart(fmla, data = X_train, method = "anova")

yhatA <- predict(mytree, newdata = X_train, type = "vector")
yhatA <- as.matrix(yhatA)

linearMod <- lm(fmla, data = X_train)
mytree <- rpart(fmla, data = X_train, method = "anova")

yhatA <- predict(linearMod, X_test)
yhatB <- predict(mytree, X_test)

y_test <- X_test[attributeNames[11]]

# --------------------------------------------
# Perform statistical comparison of the models
# --------------------------------------------

# Compute z with squared error.
zA <- abs(y_test - yhatA)**2

# Confidence interval for model A
res <- t.test(zA, alternative = "two.sided", alpha = 0.05)

# Example on how to extract confidence interval
(CIA <- c(res$conf.int[1], res$conf.int[2]))

# Compute confidence interval of z = zA-zB and p-value of Null hypothesis
zB <- abs(y_test - yhatB)**2
z <- zA - zB
t.test(z, alternative = "two.sided", alpha = 0.05)
