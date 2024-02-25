####################
# Exercise 3.1.5
####################

source("Scripts/ex3_1_4.R")
source("Tools/similarity.R")

size <- dim(dtm_stemmed)
cosmeas <- c()
q <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0)
dtmat <- matrix(dtm_stemmed, dtm_stemmed$nrow, dtm_stemmed$ncol)

for (irow in 1:size[1]) {
  doc <- dtm_stemmed[irow, ]
  cosmeas[irow] <- similarity(t(dtmat[irow, ]), t(q), 'cos')
}

print("Cosine similarity from q to docs: ")
print(cosmeas)
