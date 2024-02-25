####################
# Exercise 0.4.4
####################

# Extracting the elements from vectors is easy. Consider the
# following definition of x and the echoed results

x <- c(rep(0, 2), seq(from = 0, to = 3, length.out = 6), rep(1, 3))

# Take out elements 2 through 5 (i.e. 6 not inclusive)
x[2:6]

# Return the length of x
length(x)

# Try typing '?length'
x[length(x)] # take the last element of x

# Return every other element of x starting from the 2nd
x[seq(from = 2, by = 2, to = length(x))]

# Inserting numbers into vectors is also easy. Using the same
# definition of x and observe the results when typing
y <- x

# Notice that we're inserting the same scalar value "pi" into all elements
# that we index y with
y[seq(from = 2, by = 2, to = length(x))] <- pi

# You can also try:
# y[seq(from=2, by=2, to=length(x))] = seq(from=2, by=2, to=10)

# Observe the results when indexing the vector y with
# y[1] and y[0]. Is y[0] defined?
