# lasso 
#---------------------------------------------------------------------------------------------------
#data1
#---------------------------------------------------------------------------------------------------

rm(list=ls())
set.seed(123)
library(glmnet)

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
    Y <- -2*sin(2*data$X_5) + (data$X_6)**2 + data$X_7 + exp(-data$X_8) + epsilon
    X <- as.matrix(data)
    
    lasso_fit <- glmnet(X,Y,alpha=1) # alpha = 1 lasso et pas ridge
    
    # selection de variables (validation croissé lambda)
    cv_fit <- cv.glmnet(X, Y, alpha = 1)
    best_lambda <- cv_fit$lambda.min
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  # Exclure l'intercept
    
    # Obtenir les indices des 4 plus grandes valeurs absolues des coefficients
    selected_features <- order(abs(coefficients), decreasing = TRUE)[1:d]
    
    val_com <- intersect(selected_features,c(5,6,7,8))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_lasso1 <- matrix(0,nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_lasso1[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}


write.csv(selected_lasso1, file = "selected_lasso_data1.csv", row.names = FALSE)


#-------------------------------------------------------------------------------------------------------
#data2
#-------------------------------------------------------------------------------------------------------
rm(list=ls())
set.seed(123)

rep <- 30
# Fonction pour sélectionner 3 variables avec Lasso
Selected_features <- function(n, p, d){
  pourcentage <- numeric(rep)
  for(i in 1:rep){
    # Générer les données
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    
    epsilon <- rnorm(n, 0, 1)
    Y <- data$X_4 * exp(2 * data$X_5) + data$X_6^2 + epsilon  # Génération de la cible Y
    
    # Ajuster un modèle Lasso
    X <- as.matrix(data)  # glmnet nécessite une matrice
    lasso_fit <- glmnet(X, Y, alpha = 1)  # alpha = 1 pour Lasso
    
    # Sélection des coefficients avec le lambda minimal (validation croisée)
    cv_fit <- cv.glmnet(X, Y, alpha = 1)
    best_lambda <- cv_fit$lambda.min
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  # Exclure l'intercept
    
    # Obtenir les indices des 3 plus grandes valeurs absolues des coefficients
    selected_features <- order(abs(coefficients), decreasing = TRUE)[1:d]
    
    # Calculer le pourcentage d'intersection avec V1, V2, V3
    valeurs_communes <- intersect(selected_features, c(4,5,6))
    pourcentage[i] <- (length(valeurs_communes) / d) * 100
  }
  return(pourcentage)
}

selected_lasso2 <- matrix(nrow = 10, ncol = rep)

for(j in 1:10){
  print(j)
  selected_lasso2[j, ] <- Selected_features(25 * j, 1000, 3)
}
write.csv(selected_lasso2, file = "selected_lasso_data2.csv", row.names = FALSE)

