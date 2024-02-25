####################
# Exercise 12.1.3
####################

X <- read.csv(file.path("Data", "courses.txt"), header = FALSE, sep = ",")
X <- data.matrix(X, rownames.force = NA)
labels <- c("02322", "02450", "02451", "02453", "02454", "02457", "02459", "02582")

# Binarize the dataset, handle NA values
N <- dim(X)[1]
M <- length(labels)
Xbin <- matrix(0, nrow = N, ncol = M)
for (i in 1:N) {
  Xbin[i, X[i, !is.na(X[i, ])]] <- 1
}

# Note how the data has been transformed:
print(X)
print(Xbin)
