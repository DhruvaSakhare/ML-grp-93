####################
# Exercise 4.2.7
####################
source("Scripts/ex4_2_1.R")

# Standardize each column using the function scale (see ?scale)
X_scaled <- as.data.frame(scale(X))  

# Sums of columns are now practically zero (difference from zero is 
# due to machine error), and variance of columns are now one.
apply(X_scaled, 2, sum)
apply(X_scaled, 2, sd)

{
  par(mfrow=c(1, 1))
  image(t(X_scaled[N:1, ]), col = gray(0:32 / 32), xaxt = "n", yaxt = "n",
        ylab = "Observation number", xlab = "Attribute")
  axis(1, at = seq(from = 0, to = 1, length.out = 4), labels = attributeNames)
  axis(2, at = seq(from = 0, to = 1, length.out = N), labels = 1:N)
}
