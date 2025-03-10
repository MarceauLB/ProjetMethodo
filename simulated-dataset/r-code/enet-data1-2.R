# Elastic net 
library(glmnet)
#-----------------------------------------------------------------------------------------------
# data1
#-----------------------------------------------------------------------------------------------

rm(list=ls())
set.seed(0803)

rep <- 30
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    X <- as.matrix(data)
    
    lasso_fit <- glmnet(X,Y,alpha=0.5) # alpha = 1 lasso et pas ridge
    
    # selection de variables (validation croissé lambda)
    cv_fit <- cv.glmnet(X, Y, alpha = 0.5)
    best_lambda <- cv_fit$lambda.min
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  # Exclure l'intercept
    
    # Obtenir les indices des 4 plus grandes valeurs absolues des coefficients
    selected_features <- order(abs(coefficients), decreasing = TRUE)[1:d]
    
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feat_EN <- matrix(0,nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_feat_EN[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}

rmean <- rowMeans(selected_feat_EN)
echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,type="l",
     ylim = range(0,1))

write.csv(selected_feat_EN, file = "selected_ENet_data1.csv", row.names = FALSE)




#-----------------------------------------------------------------------------------------------
# data2
#-----------------------------------------------------------------------------------------------
rm(list=ls())
set.seed(123)

rep <- 30
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n, 0, 1)
    Y <- data$X_1 * exp(2 * data$X_2) + data$X_3^2 + epsilon  # Génération de la cible Y
    
    X <- as.matrix(data)  # glmnet nécessite une matrice
    lasso_fit <- glmnet(X, Y, alpha = 0.5)  # alpha = 1 pour Lasso
    
    # Sélection des coefficients avec le lambda minimal (validation croisée)
    cv_fit <- cv.glmnet(X, Y, alpha = 0.5)
    best_lambda <- cv_fit$lambda.min
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  # Exclure l'intercept
    
    # Obtenir les indices des 3 plus grandes valeurs absolues des coefficients
    selected_features <- order(abs(coefficients), decreasing = TRUE)[1:d]
    
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feat_EN_2 <- matrix(0,nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_feat_EN_2[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 1000,3)
}

rmean <- rowMeans(selected_feat_EN_2)
echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,type="l",ylim = range(0,1))

write.csv(selected_feat_EN_2, file = "selected_ENet_data2.csv", row.names = FALSE)
