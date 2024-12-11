setwd("~/00_Ensai/projet-methodo/ProjetMethodo/r-code/exp_data2_r/")


install.packages('GSelection')
library(GSelection)

rm(list=ls())

n <- 100
p <- 1000

set.seed(123)

data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
epsilon <- rnorm(n,0,1)
Y <- data$V1*exp(2*data$V2) + data$V3**2 + epsilon


fit <- feature.selection(data,Y,d=3)
fit$hsic_selected_feature_index
valeurs_communes <- intersect(fit$hsic_selected_feature_index, c(1,2,3))
pourcentage <- (length(valeurs_communes) / 3) * 100


result_matrix <- matrix(nrow = 10, ncol = 30)
n <- 25

Selected_features <- function(n,p,d){
  pourcentage <- numeric(30)
  for(i in 1:30){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    epsilon <- rnorm(n,0,1)
    Y <- data$V1*exp(2*data$V2) + data$V3**2 + epsilon
  
    fit <- feature.selection(data,Y,d=3)
    fit$hsic_selected_feature_index
  
    valeurs_communes <- intersect(fit$hsic_selected_feature_index, c(1,2,3))
    pourcentage[i] <- (length(valeurs_communes) / 3) * 100
  }
  return(pourcentage)
}

for(j in 1:10){
  result_matrix[j, ] <- Selected_features(25*j,1000,3)
}

plot(seq(25,250, by = 25),
     rowMeans(result_matrix),
     type = 'line')



result_matrix2 <- matrix(nrow = 10, ncol = 30)
n <- 25

Selected_features <- function(n,p,d){
  pourcentage <- numeric(30)
  for(i in 1:30){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    epsilon <- rnorm(n,0,1)
    Y <- data$V1*exp(2*data$V2) + data$V3**2 + epsilon
  
    fit <- feature.selection(data,Y,d=3)
    fit$hsic_selected_feature_index
  
    valeurs_communes <- intersect(fit$spam_selected_feature_index, c(1,2,3))
    pourcentage[i] <- (length(valeurs_communes) / 3) * 100
  }
  return(pourcentage)
}


for(j in 1:10){
  result_matrix2[j, ] <- Selected_features(25*j,1000,3)
}

plot(seq(25,250, by = 25),
     rowMeans(result_matrix2),
     type = 'line')




####################################################################
# On trace tous les résultats sur un même graphique 
####################################################################
rm(list=ls())
res_lasso <- read.csv("selected_LASSO.csv")
res_spam <- read.csv("selected_spam.csv")
res_mrmr <- read.csv("selected_mRMR.csv")
res_hsic <- read.csv("selected_hsic.csv")


mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)

echantillon_points <- seq(25, 250, by = 25)

plot(echantillon_points, mean_row_hsic / 100, type = "l", 
     xlab = "Taille de l'échantillon", 
     ylab = "Moyenne des lignes",
     main = "Moyenne des lignes par taille d'échantillon",
     col = "red", lwd = 1)  # Rouge pour Lasso

# Ajouter les autres séries
lines(echantillon_points, mean_row_spam/100, col = "blue", lwd = 1)  # Bleu pour Spam
lines(echantillon_points, mean_row_mrmr/100, col = "darkgreen", lwd = 1) # Vert pour mRMR
lines(echantillon_points, mean_row_lasso/100, col = "purple", lwd = 1) # Violet pour HSIC

# Ajouter une légende pour identifier les couleurs
legend("topleft", legend = c("HSIC", "Spam", "mRMR", "Lasso"), 
       col = c("red", "blue", "darkgreen", "purple"), lwd = 1,cex=0.8)



