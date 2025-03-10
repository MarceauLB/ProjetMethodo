
#-----------------------------------------------------------------------
# Spam Selections 
#-----------------------------------------------------------------------
rm(list=ls())

n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(123)  # Pour reproduire les résultats

prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(30)
  for(i in 1:30){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    X <- as.matrix(data)
    a <- feature.selection(X,Y,4)
    val_com <- intersect(a$spam_selected_feature_index,c(1,2,3,4))
    list_pourcentage[i] <- (length(val_com)/4)*100
  }
  return((list_pourcentage))
}

selected_feature_n <- matrix(nrow = 16,ncol = 30)
seq(25, 250, by = 15)
length(seq(25, 250, by = 15))

for(taille_echantillon in 1:16){
  selected_feature_n[taille_echantillon,] <- prc_selected_features(10+taille_echantillon*25, 256,4)
}

write.csv(selected_feature_n, file = "selected_spam.csv", row.names = FALSE)


#-----------------------------------------------------------------------
# mRMR Selections 
#-----------------------------------------------------------------------

rm(list=ls())
library(mRMRe)
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(123)  # Pour reproduire les résultats

prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(30)
  for(i in 1:30){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    data$Y <- Y
    mrmr_data <- mRMR.data(data = data)
    fit <- mRMR.classic(data = mrmr_data,target_indices=ncol(data),feature_count = d)
    selected_features <- as.numeric(fit@filters[[1]])  # Indices sélectionnés
    val_com <- intersect(selected_features,c(1,2,3,4))
    list_pourcentage[i] <- (length(val_com)/4)*100
  }
  return((list_pourcentage))
}

selected_feature_mrmr <- matrix(nrow = 16,ncol = 30)
seq(25, 250, by = 15)
length(seq(25, 250, by = 15))

for(taille_echantillon in 1:16){
  selected_feature_mrmr[taille_echantillon,] <- prc_selected_features(10+taille_echantillon*25, 256,4)
}

write.csv(selected_feature_mrmr, file = "selected_mrmr.csv", row.names = FALSE)


