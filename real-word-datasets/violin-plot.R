data <- read.csv("00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/dataset_csv/ar10p.csv")

X <- as.matrix(data)
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- X[,-(1:p)]
X <- X[,-(p+1)]
table(Y)

vec_X <- as.vector(X[1:n,1:p])
summary(vec_X)
var(vec_X)
sqrt(var(vec_X))


library(ggplot2)
data <- data.frame(Value = vec_X)
ggplot(data, aes(x = "", y = Value)) + 
  geom_violin(fill = "skyblue", color = "black") + 
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a boxplot inside for more info
  labs(title = "Violin Plot of vec_X", y = "Values") +
  theme_minimal()

X_scaled <- as.data.frame(scale(X,center = TRUE,scale = TRUE))
vec_X_scaled <- as.vector(as.matrix(X_scaled))
summary(vec_X_scaled)
var(vec_X_scaled)
sqrt(var(vec_X_scaled))

boxplot(vec_X_scaled)
data2 <- data.frame(Value = vec_X_scaled)
ggplot(data2, aes(x = "", y = Value)) + 
  geom_violin(fill = "skyblue", color = "black") + 
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a boxplot inside for more info
  labs(title = "Violin Plot of vec_X", y = "Values") +
  theme_minimal()
