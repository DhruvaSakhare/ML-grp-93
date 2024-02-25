####################
# Exercise 2.1.5
####################
source("Scripts/ex2_1_1.R")

Y <- t(apply(X, 1, "-", colMeans(X))) # subtract the column means form columns of X

# Computing PCA:
pca <- prcomp(X)
V <- pca$rotation
Z <- pca$x

# We saw in 2.1.3 that the first 3 components explained more than 90
# percent of the variance. Let's look at their coefficients:
pcs <- 1:3
test <- as.data.frame(melt(data.table(V[, pcs])))
ggplot(test, aes(x = rep(1:8, length(pcs)), y = value, fill=variable)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(fill="PC", x = "Attributes", y = "Component coefficients")

# Inspecting the plot, we see that the 2nd principal component has large
# (in magnitude) coefficients for attributes A, E and H. We can confirm
# this by looking at it's numerical values directly, too:
cat("PC2:", V[, 1])

# How does this translate to the actual data and its projections?
# Looking at the data for water:

# Projection of water class onto the 2nd principal component.
water <- Y[y == 4, ]
cat("First water observation:", water[1, ])

# Based on the coefficients and the attribute values for the observation
# displayed, would you expect the projection onto PC2 to be positive or
# negative - why? Consider *both* the magnitude and sign of *both* the
# coefficient and the attribute!

# You can determine the projection by (remove comments):
print("...and its projection onto PC2")
print(as.numeric(t(water[1, ]) %*% V[, 2]))

# Try to explain why.
# This is equivalent to:
Z[1, 2]

