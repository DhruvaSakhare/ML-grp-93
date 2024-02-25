####################
# Exercise 2.1.2
####################

# Run ex2.1.1:
source('Scripts/ex2_1_1.R')

# Choose which sensors to plot
i = 1
j = 2

# Make simple plot
plot(X[ , i], X[ , j])

## Make more fancy plot
library(ggplot2)
ggplot() + geom_point(aes(x = X[ , i], y = X[ , j], color = classLabels), size=4, alpha=0.5) +
  theme(legend.position = c(0.88, 0.77), legend.title= element_blank()) +
  labs(x = attributeNames[i], y = attributeNames[j])

