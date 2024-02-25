####################
# Exercise 3.3.2
####################
source("Tools/similarity.R")

# Generate two data objects with M random (standard uniformly distributed) attributes
M = 5;
x = as.matrix(runif(M));
y = as.matrix(runif(M));


# Two constants
a = 1.5;
b = 1.5;

# Check the statements in the exercise
similarity(x,y,'cos') - similarity(a*x,y,'cos')
similarity(x,y,'ext') - similarity(a*x,y,'ext')
similarity(x,y,'cor') - similarity(a*x,y,'cor')
similarity(x,y,'cos') - similarity(b+x,y,'cos')
similarity(x,y,'ext') - similarity(b+x,y,'ext')
similarity(x,y,'cor') - similarity(b+x,y,'cor')
