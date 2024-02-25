####################
# Exercise 5.1.6
####################
source("Scripts/ex5_1_5.R")

library(rpart)
X_frame <- data.frame(X)
colnames(X_frame) <- attributeNames
classassignments <- classNames[y + 1]

# Construct formula to fit automatically to avoid typing in each variable name
(fmla <- as.formula(paste("classassignments ~ ", paste(attributeNames, collapse = "+"))))

# Fit classification tree
mytree <- rpart(fmla, data = X_frame,
                control = rpart.control(minsplit = 100, minbucket = 1, cp = 0),
                parms = list(split = "gini"), method = "class")

par(mfrow=c(1,1), xpd = NA)
plot(mytree)
text(mytree)
