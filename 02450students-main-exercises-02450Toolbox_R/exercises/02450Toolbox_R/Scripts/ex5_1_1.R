####################
# Exercise 5.1.1
####################
rm(list = ls()) # Clear work space

# Names of data objects
dataobjectNames <- c(
  "Human",
  "Python",
  "Salmon",
  "Whale",
  "Frog",
  "Komodo dragon",
  "Bat",
  "Pigeon",
  "Cat",
  "Leopard shark",
  "Turtle",
  "Penguin",
  "Porcupine",
  "Eel",
  "Salamander"
)

# Attribute names
attributeNames <- make.names(c(
  "Body temperature",
  "Skin cover",
  "Gives birth",
  "Aquatic creature",
  "Aerial creature",
  "Has legs",
  "Hibernates"
))

# Attribute values
X <- rbind(
  c(1, 1, 1, 0, 0, 1, 0),
  c(0, 2, 0, 0, 0, 0, 1),
  c(0, 2, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 0),
  c(0, 0, 0, 2, 0, 1, 1),
  c(0, 2, 0, 0, 0, 1, 0),
  c(1, 1, 1, 0, 1, 1, 1),
  c(1, 3, 0, 0, 1, 1, 0),
  c(1, 4, 1, 0, 0, 1, 0),
  c(0, 2, 1, 1, 0, 0, 0),
  c(0, 2, 0, 2, 0, 1, 0),
  c(1, 3, 0, 2, 0, 1, 0),
  c(1, 5, 1, 0, 0, 1, 1),
  c(0, 2, 0, 1, 0, 0, 0),
  c(0, 0, 0, 2, 0, 1, 1)
)
# Class indices
y <- c(3, 4, 2, 3, 0, 4, 3, 1, 3, 2, 4, 1, 3, 2, 0)

# Class names
classNames <- c("Amphibian", "Bird", "Fish", "Mammal", "Reptile")

# Number data objects, attributes, and classes
N <- dim(X)[1]
M <- length(attributeNames)
C <- length(classNames)
