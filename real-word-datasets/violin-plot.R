


data <- read.csv("00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/dataset_csv/ar10p.csv")
table(data$y)
X <- as.matrix(data)
dim(X)
vec_X <- as.vector(X[1:(dim(X)[1]),1:(dim(X)[2]-1)])
summary(vec_X)
sqrt(var(vec_X))
boxplot(vec_X)

library(ggplot2)
data <- data.frame(Value = vec_X)
ggplot(data, aes(x = "", y = Value)) + 
  geom_violin(fill = "skyblue", color = "black") + 
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a boxplot inside for more info
  labs(title = "Violin Plot of vec_X", y = "Values") +
  theme_minimal()
