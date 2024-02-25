####################
# Exercise 3.2.1
####################

rm(list = ls()) # Clear work space

x <- c(-0.68, -2.11, 2.39, 0.26, 1.46, 1.33, 1.03, -0.41, -0.33, 0.47)

mean(x)
sd(x)
median(x)
diff(range(x))

# Range returns the minimum and maximum of the vector x.
# To get the range, we must take the maximum minus the minimum.
# We do this using the function diff, which finds differences
# between consecutive elements in a vector.
