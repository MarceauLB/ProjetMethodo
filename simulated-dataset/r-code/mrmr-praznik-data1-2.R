# mrmr test 
rm(list=ls())
library(praznik)

#lineaire
n <- 250 
p <- 256
X <- as.data.frame(matrix(rnorm(n*p,0,1),nrow=n,ncol=p))
eps <- rnorm(n,0,1)
Y_lin <- -2*sin(2*X[,1]) +X[,2]**2 + X[,3] + exp(-X[,4]) + eps

?MRMR
res <- MRMR(X,Y_lin,4)
res


# non lineaire 
n <- 500
p <- 50
X <- as.data.frame(matrix(rnorm(n*p,0,1),nrow=n,ncol=p))
eps <- rnorm(n,0,1)
Y_non_lin <- X[,1]*exp(2*X[,2]) + X[,3]**2 + eps
res <- MRMR(X,Y_non_lin,3)
res
a <- as.data.frame(res$selection)
a$`res$selection`
intersect(a$`res$selection`,c(1,2,3))



## applications data1
rm(list=ls())
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(0803)  # Pour reproduire les résultats
rep <- 30
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    mrmr_data <- MRMR(data,Y,d)
    feat_select_mrmr <- as.data.frame(mrmr_data$selection)
    selected_features <- feat_select_mrmr$`mrmr_data$selection`
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feature_mrmr <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  selected_feature_mrmr[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}
rmean <- rowMeans(selected_feature_mrmr)
echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,type="l")

write.csv(selected_feature_mrmr, file = "selected_mrmr_prazdata1.csv", row.names = FALSE)



## application data2 
rm(list=ls())
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(0803)  # Pour reproduire les résultats
rep <- 30
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:30){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon
    mrmr_data <- MRMR(data,Y,d)
    feat_select_mrmr <- as.data.frame(mrmr_data$selection)
    selected_features <- feat_select_mrmr$`mrmr_data$selection`
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feature_mrmr2 <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_feature_mrmr2[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 1000,3)
}
rmean <- rowMeans(selected_feature_mrmr2)
echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,ylim = c(0,1),type="l")

write.csv(selected_feature_mrmr2, file = "selected_mrmr_prazdata2.csv", row.names = FALSE)

