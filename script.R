install.packages("corrplot")
install.packages("ggfortify")
library(corrplot)
library(ggfortify)

data <- read.csv("ecoli.data", header = FALSE, row.names = 1)

#remove the binary attributes
data.refined <- data[ -c(3:4) ]
colnames(data.refined) <- c('mcg','gvh','aac','alm1','alm2','class') 

data.numeric <- data.refined[, sapply(data.refined, is.numeric)]

#Plots
for (col in colnames(data.numeric)) {
  hist(data.numeric[[col]], main = paste("Histogram of", col))
  qqnorm(data.numeric[[col]], main = paste("Q-Q Plot of", col))
  qqline(data.numeric[[col]])
}
# Shapiro-Wilk Test
shapiro_results <- sapply(data.numeric, shapiro.test)
print(shapiro_results)

#Correaltion test
correlation_matrix <- cor(data.numeric)
print(correlation_matrix)

# Plot the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color")

boxplot(data.numeric, main = "Box Plot of Data")


#PCA

#Standardization
data.numeric.std <- as.data.frame(scale(data.numeric))

# PCA
pca_res <- prcomp(data.numeric.std, scale. = TRUE)
autoplot(pca_res)

autoplot(pca_res, data = data.refined, colour = 'class')
autoplot(pca_res, data = data.refined, colour = 'class', loadings = TRUE)

# Calculate the cumulative proportion of variance explained
cumulative_variance <- cumsum(pca_res$sdev^2) / sum(pca_res$sdev^2)

# Plot the cumulative proportion of variance explained
plot(cumulative_variance, type = "b", 
     xlab = "Number of Components", ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Proportion of Variance Explained by PCA Components")


