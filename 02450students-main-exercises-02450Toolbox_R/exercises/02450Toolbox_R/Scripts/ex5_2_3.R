####################
# Exercise 5.2.3
####################

rm(list = ls()) # Clear work space

# Number of data objects
N <- 50
# Number of non-linear terms in generated data
Kd <- 2
# Number of non-linear terms in model
Km <- 2
# Noise variance
s <- 0.5
# X-values to evaluate model and fit
Xe <- seq(from = -1, to = 1, length.out = 1000)

## Data set generation
##------------------------------------------------------------------------------

# Attribute values
X <- seq(from = -1, to = 1, length.out = N)
Xd <- matrix(sapply(X, FUN = "^", 1:Kd), nrow = N, byrow = TRUE)
Xde <- matrix(sapply(Xe, FUN = "^", 1:Kd), nrow = 1000, byrow = TRUE)

# Noise
epsilon <- rnorm(mean = 0, sd = s, n = N)

# Model parameters
w <- -(-.9)^(1:(Kd + 1))

# Outputs
y <- cbind(rep(1, times = N), Xd) %*% w + epsilon

## Model to fit
##------------------------------------------------------------------------------

# Attribute values
Xm <- matrix(sapply(X, FUN = "^", 1:Km), nrow = N, byrow = TRUE)
Xme <- matrix(sapply(Xe, FUN = "^", 1:Km), nrow = 1000, byrow = TRUE)

# Estimate model parameters
data <- data.frame(Xm)
xnam <- paste("X", 1:Km, sep = "")
colnames(data) <- xnam
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse = "+"))))
w_est <- lm(fmla, data = data)

{
  # Plot the predictions of the model
  plot(X, y, main = "Linear regression", xlab = "X", ylab = "y", ylim = c(-2, 8))
  newdata <- data.frame(Xme)
  colnames(newdata) <- xnam
  y_est <- predict(w_est, newdata = newdata)
  lines(Xe, y_est, col = "red")
  y_true <- w[1] + Xde %*% matrix(w[(1:Kd) + 1], ncol = 1)
  lines(Xe, y_true, col = "green")
  legend("topleft", legend = c("Data", "Fitted model", "True model"),
         fill = c("black", "red", "green"))
}
