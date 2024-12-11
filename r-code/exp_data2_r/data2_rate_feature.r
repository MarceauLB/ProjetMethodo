setwd("~/00_Ensai/projet-methodo/ProjetMethodo/r-code/exp_data1_r/")


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