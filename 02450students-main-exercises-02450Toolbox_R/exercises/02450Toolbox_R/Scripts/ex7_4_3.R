####################
# Exercise 7.4.3
####################
rm(list = ls()) # Clear work space

## Read male and female names and extract features
male <- read.table(file.path("Data", "male.txt"), sep = "\r")
female <- read.table(file.path("Data", "female.txt"), sep = "\r")

extractNameFeatures <- function(name) {
  name <- as.character(name)
  name <- tolower(name)
  l <- nchar(name)
  name <- strsplit(name, "")
  c(name[[1]][1], name[[1]][2], name[[1]][l - 1], name[[1]][l])
}

asc_single <- function(x) {
  strtoi(charToRaw(x), 16L)
}

asc <- function(x) {
  sapply(x, asc_single)
}

# Extract male name features
Xmale <- t(sapply(male$V1, extractNameFeatures))
Xmale <- t(apply(Xmale, 1, asc) - asc("a") + 1)

# Extract female name features
Xfemale <- t(sapply(female$V1, extractNameFeatures))
Xfemale <- t(apply(Xfemale, 1, asc) - asc("a") + 1)

# Concatenate male and female
X <- rbind(Xmale, Xfemale)

# Make class indices etc.
y <- c(rep(0, times = dim(Xmale)[1]), rep(1, times = dim(Xfemale)[1]))
N <- dim(X)[1]
M <- dim(X)[2]
C <- 2
attributeNames <- make.names(c("First Letter", "Second Letter", "Next-to-last Letter", "Last letter"))
classNames <- c("Female", "Male")

X <- data.frame(X)
colnames(X) <- attributeNames
