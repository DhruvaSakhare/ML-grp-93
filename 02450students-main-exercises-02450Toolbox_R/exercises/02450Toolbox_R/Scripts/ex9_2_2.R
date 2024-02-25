####################
# Exercise 9.2.2
####################
rm(list = ls()) # Clear work space

# Package for Cross-Validation
library(caret)

# Load data
library(R.matlab)
data <- readMat(file.path("Data", "wine2.mat"))
X <- data$X
N <- data$N
attributeNames <- make.names(unlist(data$attributeNames))
M <- data$M
y <- data$y
C <- data$C
classNames <- as.vector(unlist(data$classNames))

X <- data.frame(X)
colnames(X) <- attributeNames

# Remove all attributes except "Alcohol"
i <- 11 # Attribute indices to keep

#################
# Crossvalidation
#################

# Create crossvalidation partition for evaluation
# using 50 pct. split between training and test

# For reproducibility
set.seed(1234)

CV <- list()
CV$which <- createFolds(y, k = 2, list = F)
CV$TrainSize <- c()
CV$TestSize <- c()

# Extract the training and test set
X_train <- X[CV$which != 1, ]
y_train <- y[CV$which != 1]
X_test <- X[CV$which == 1, ]
y_test <- y[CV$which == 1]
CV$TrainSize[1] <- length(y_train)
CV$TestSize[1] <- length(y_test)

## Fit model
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames[i], collapse = "+"))))

# Fit logistic regression model to predict the type of wine
w_est <- glm(fmla, family = binomial(link = "logit"), data = X_train)

# Evaluate the logistic regression on the test data
p <- predict(w_est, newdata = X_test, type = "response")

####################################################
# Plot receiver operating characteristic (ROC) curve
####################################################

# Calculate ROC curve
frame <- data.frame(Prob = 1 - p, Truth = factor(y_test))
roccurve <- roc_curve(frame, Truth, Prob)

# Calculate AUC
auc <- roc_auc(frame, Truth, Prob)$.estimate

# Plot ROC curve
ggplot(roccurve, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 0.75, color="red") +
  geom_abline(slope = 1, intercept = 0, size = 1) +
  coord_fixed() + labs(y = "True positive rate (Sensitivity)",
                       x = "False positive rate (1 - Specificity)") +
  ggtitle(paste0("ROC Curve (AUC = ", round(auc, 3), ")")) + 
  theme(plot.title = element_text(hjust = 0.5))



#######################
# Plot confusion matrix
#######################

# We'll plot a confusion matrix as well

y_test_est <- p > .5 # Threshold

tr <- factor(classNames[as.numeric(y_test) + 1], levels=classNames)
pr <- factor(classNames[as.numeric(y_test_est) + 1], levels=classNames)

cm <- confusionMatrix(data = pr, reference = tr)
values <- as.numeric(cm$table)
accuracy <- cm$overall[1]
error_rate <- 1 - accuracy

TClass <- rep(classNames, each = C)
PClass <- rep(classNames, times = C)

df <- data.frame(TClass, PClass, values)

par(mfrow=c(1,1))

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

