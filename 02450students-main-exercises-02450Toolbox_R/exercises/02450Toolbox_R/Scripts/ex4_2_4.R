####################
# Exercise 4.2.4
####################
source("Scripts/ex4_2_1.R")

# Done with base R plot
yvals <- c()
for (m in 0:(C - 1))
{
  res <- boxplot(X[m == y, ], plot = FALSE)
  yvals <- rbind(yvals, res$stats)
}

{
  par(mfrow = c(1, 3))
  for (m in 0:(C - 1)) {
    boxplot(X[m == y, ], main = paste("Boxplot for", classNames[m + 1]), ylim = c(min(yvals), max(yvals)))
  }
}

# ------------
# With ggplot2
# ------------

library(ggplot2)

# Reorganize data with tidyr::gather
new_data <- gather(cbind(X, y), key, value, -y)

ggplot(gather(cbind(X, y), key, value, -y), aes(x = key, y = value)) +
  geom_boxplot() + facet_wrap(~classNames[y+1]) + labs(y = "Value") +
  theme(axis.text.x = element_text(angle = -25))

