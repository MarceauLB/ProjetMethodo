rm(list=ls())

data <- read.csv("00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/dataset_csv/ar10p.csv")

X <- as.matrix(data)
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- X[,-(1:p)]
X <- X[,-(p+1)]
table(Y)

X <- as.matrix(X)

# Define the image dimensions (choose correct factors of 2400)
image_height <- 40  # Adjust based on correct aspect ratio
image_width <- 60

# Verify that the dimensions match
if (image_height * image_width != ncol(X)) {
  stop("Error: Selected dimensions do not match the number of features!")
}

# Select a sample index to visualize
sample_idx <- 1  # Modifier pour voir d'autres images

# Reshaper la ligne en une matrice image
sample_image <- matrix(X[sample_idx, ], nrow = image_height, ncol = image_width, byrow = TRUE)

# Option 1 : Rotation 90° anti-horaire
rotated_image <- t(apply(sample_image, 1, rev))

# Affichage de l'image
image(rotated_image, col = gray.colors(256), axes = FALSE)
title(main = paste("Image du sample", sample_idx))


selected_pic <- c(1,2,4,14,15,17)
par(mfrow=c(2,3))  # Affiche 6 images (2 lignes, 3 colonnes)
for (i in selected_pic) {
  sample_image <- matrix(X[i, ], nrow = image_height, ncol = image_width, byrow = TRUE)
  rotated_image <- t(apply(sample_image, 2, rev))
  image(rotated_image, col = gray.colors(256), axes = FALSE)
  title(main = paste("Image", i))
}
par(mfrow=c(1,1))  # Réinitialise la mise en page



## 
vec_X <- as.vector(X[1:n,1:p])
summary(vec_X)
var(vec_X)
sqrt(var(vec_X))


library(ggplot2)
data <- data.frame(Value = vec_X)
ggplot(data, aes(x = "", y = Value)) + 
  geom_violin(fill = "cyan3", color = "black") + 
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Adds a boxplot inside for more info
  labs(title = "Violin Plot de la luminosité de l'ensemble des pixels", y = "Valeurs",x="") +
  theme_minimal()



## 

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
