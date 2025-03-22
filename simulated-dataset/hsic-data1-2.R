#-----------------------------------------------------------------------
# Performance of HSIC  
#-----------------------------------------------------------------------
rm(list=ls())
# install.packages('GSelection')
library(GSelection)

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
    X <- as.matrix(data)
    a <- feature.selection(X,Y,d)
    # print(a$hsic_selected_feature_index)
    val_com <- intersect(a$hsic_selected_feature_index,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_hsic_1 <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_hsic_1[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}
rmean <- rowMeans(selected_hsic_1)

write.csv(selected_hsic_1, file = "selected_hsic_data1.csv", row.names = FALSE)


#-----------------------------------------------------------------------
# HSIC LASSO  data2
#-----------------------------------------------------------------------
rm(list=ls())
set.seed(123)

rep <- 30
Selected_features <- function(n,p,d){
  pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon
    X <- as.matrix(data)
    
    fit <- feature.selection(data,Y,d=3)
    fit$hsic_selected_feature_index
    
    valeurs_communes <- intersect(fit$hsic_selected_feature_index, c(1:d))
    pourcentage[i] <- (length(valeurs_communes)/d) * 100
  }
  return(pourcentage)
}

selected_hsic_2 <- matrix(nrow = 10, ncol = rep)

for(j in 1:10){
  selected_hsic_2[j, ] <- Selected_features(25*j,1000,3)
}
rmean2 <- rowMeans(selected_hsic_2)

write.csv(selected_hsic_2, file = "selected_hsic_data2.csv", row.names = FALSE)
