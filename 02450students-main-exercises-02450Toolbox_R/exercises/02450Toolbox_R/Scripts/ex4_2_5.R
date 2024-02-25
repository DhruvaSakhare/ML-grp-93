####################
# Exercise 4.2.5
####################
source("Scripts/ex4_2_1.R")

observationColors <- c("blue", "green3", "red")[unclass(y+1)]
{
  par(xpd=TRUE)
  pairs(X, bg=observationColors, pch=21)
  legend(0, 1, classNames, fill=unique(observationColors))
}

# ------------
# With ggplot2
# ------------
library(ggplot2)
library(GGally) # install.packages("GGally")

ggpairs(X)

