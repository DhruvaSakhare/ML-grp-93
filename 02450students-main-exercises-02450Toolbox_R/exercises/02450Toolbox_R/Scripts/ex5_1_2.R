####################
# Exercise 5.1.2
####################

source("Scripts/ex5_1_1.R")

# If you do not already have the package "rpart" installed,
# install it with the command install.packages("rpart").
library(rpart)

X_frame <- data.frame(X)
colnames(X_frame) <- attributeNames
X_frame[, attributeNames] <- lapply(X_frame[, attributeNames], factor)
# Check that X_frame represents data as categorical variables
summary(X_frame)

# Fit classification tree
formula <- classNames[y + 1] ~ Body.temperature + Skin.cover +
  Gives.birth + Aquatic.creature + Aerial.creature +
  Has.legs + Hibernates

mytree <- rpart(formula, data = X_frame,
                control = rpart.control(minsplit = 1, minbucket = 0, cp = 0),
                parms = list(split = "gini"), method = "class")

par(mfrow=c(1,1), xpd = NA) # Make room for text labels
plot(mytree)
text(mytree, pretty = 0)
# pretty = 0 makes attribute values show up as the numerical values
# they take in the data matrix X instead of encoding using a, b, c, etc.

# Inspect details of tree
summary(mytree)

