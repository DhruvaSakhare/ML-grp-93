dbplot <- function(X, attributeNames, predictionFunction, y, contourLevels = 0.5, contourCols = "white", cols = c(), ...) {
  # DBPLOT Plots a decision boundary for a classification problem with two attributes.
  #
  # Usage:
  #    dbplot(X, attributeNames, predictionFunction, y, contourLevels=0.5, contourCols='white',   cols= c(),...)
  #  N is the number of data objects and M is the number of attributes.
  #    X: N-by-M matrix of attribute values
  #    attributeNames: vector of attributenames
  #    predictionFunction: trained function that predicts response from data in X
  #    y: N-by-1 vector of class labels or probabilities of belonging to one of the classes.
  #    contourLevels: values that separate classes
  #    contourCols: color used to draw boundaries between classes
  #    cols: colors used to shade the areas belonging to the different classes according to the prediction function. Only used in the non-binary case. In the binary case, the colors used are always blue and green.
  #
  #    If the number of attributes is M=2, the decision boundary is plotted
  #    in the attribute space. Otherwise it is plotted in the space spanned by the two first principal components.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
  
  suppressWarnings({

  if (length(contourCols) == 1 && length(unique(y)) != 2) {
    contourCols <- rep(contourCols, times = length(unique(y)))
  }

  if (length(contourLevels) == 1 && length(unique(y)) != 2) {
    contourLevels <- rep(contourLevels, times = length(unique(y)))
  }

  if (length(contourLevels) != length(contourCols)) {
    stop("The arguments contourLevels and contourCols to the function dbplot must have the same length.")
  }

  if (length(cols) == 0) {
    cols <- rainbow(length(unique(y)))
  }

  doPCA <- FALSE
  if (dim(X)[2] > 2) {
    doPCA <- TRUE
    means <- colMeans(X)
    Xzeromean <- t(apply(X, 1, "-", means))
    svdres <- svd(Xzeromean)

    # extract the matrices containing the left and right singular vectors, respectively
    U <- svdres$u
    V <- svdres$v
    S <- diag(svdres$d)
    X <- U %*% S
  }
  NGrid <- 200
  minx <- min(X[, 1])
  maxx <- max(X[, 1])
  seqx <- seq(from = minx, to = maxx, length.out = NGrid)
  miny <- min(X[, 2])
  maxy <- max(X[, 2])
  seqy <- seq(from = miny, to = maxy, length.out = NGrid)


  if (doPCA) {
    mat <- cbind(rep(seqx, each = length(seqy)), rep(seqy, times = length(seqx)), array(rep(0, times = (NGrid^2) * (dim(X)[2] - 2)), dim = c(NGrid^2, dim(X)[2] - 2))) %*% t(V)
    mat <- t(apply(mat, 1, "+", means))
  } else {
    mat <- matrix(c(rep(seqx, each = length(seqy)), rep(seqy, times = length(seqx))), ncol = 2)
  }


  Xgriddf <- data.frame(mat)
  colnames(Xgriddf) <- attributeNames

  probs_vector <- predictionFunction(Xgriddf, ...)
  probs <- matrix(probs_vector, nrow = length(seqx), byrow = FALSE)

  sortedclasses <- sort(as.numeric(unique(y)), decreasing = TRUE, index.return = TRUE)
  classes <- sortedclasses$x
  order <- sortedclasses$ix

  if (doPCA) {
    attributeNames <- c("PC 1", "PC 2")
  }
 
  if (all(0 <= probs) && all(probs <= 1)) {
    image(seqx, seqy, t(probs), col = rev(rainbow(200, start = 2 / 6, end = 4 / 6)), xlab = attributeNames[1], ylab = attributeNames[2], main = "Decision boundary")
    cols <- c("green", "blue")
  } else {
    image(seqx, seqy, t(probs), col = rev(cols), xlab = attributeNames[1], ylab = attributeNames[2], main = "Decision boundary")
    contour(seqx, seqy, t(probs), levels = contourLevels, labels = "", add = TRUE, type = "l", lwd = 5, col = contourCols)
  }
  darkercols <- col2rgb(cols) * 0.7
  darkercols <- rgb(t(darkercols), maxColorValue = 255)

  for (iclass in 1:length(classes)) {
    points(X[y == classes[iclass], 1], X[y == classes[iclass], 2], col = darkercols[iclass], pch = 20)
  }
  })
}
