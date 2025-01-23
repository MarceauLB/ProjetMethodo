setwd("~/00_Ensai/projet-methodo/ProjetMethodo/r-code/exp_data2_r/")


#-----------------------------------------------------------------------
# data 2: Y = X1 * exp(2*X2) + X3**2 + eps 
#-----------------------------------------------------------------------
library(GSelection)
# Exemple de génération de données 
rm(list=ls())
n <- 100
p <- 1000

set.seed(123)

data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
colnames(data) <- paste0("X_", 1:p)
head(data)

epsilon <- rnorm(n,0,1)
Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon


#-----------------------------------------------------------------------
# HSIC LASSO 
#-----------------------------------------------------------------------
rm(list=ls())

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

result_matrix <- matrix(nrow = 10, ncol = 30)
n <- 25

for(j in 1:10){
  result_matrix[j, ] <- Selected_features(25*j,1000,3)
}

write.csv(result_matrix, file = "selected_hsic.csv", row.names = FALSE)

#-----------------------------------------------------------------------
# SPAM 
#-----------------------------------------------------------------------
rm(list=ls())

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

write.csv(result_matrix2, file = "selected_spam.csv", row.names = FALSE)

#-----------------------------------------------------------------------
# Lasso classique 
#-----------------------------------------------------------------------

rm(list=ls())

result_matrix <- matrix(nrow = 10, ncol = 30)

# Fonction pour sélectionner 3 variables avec Lasso
Selected_features <- function(n, p, d) {
  pourcentage <- numeric(30)
  for (i in 1:30) {
    # Générer les données
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    epsilon <- rnorm(n, 0, 1)
    Y <- data$V1 * exp(2 * data$V2) + data$V3^2 + epsilon  # Génération de la cible Y
    
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
    valeurs_communes <- intersect(selected_features, c(1, 2, 3))
    pourcentage[i] <- (length(valeurs_communes) / d) * 100
  }
  return(pourcentage)
}

for (j in 1:10) {
  result_matrix[j, ] <- Selected_features(25 * j, 1000, 3)
}
write.csv(result_matrix, file = "selected_LASSO", row.names = FALSE)



#-----------------------------------------------------------------------
# mRMR
#-----------------------------------------------------------------------
rm(list=ls())
library(mRMRe)

result_matrix3 <- matrix(nrow = 10, ncol = 30)

# Fonction pour sélectionner des caractéristiques avec mRMR
Selected_features <- function(n, p, d) {
  pourcentage <- numeric(30)  # Pourcentage de caractéristiques communes
  for (i in 1:30) {
    # Générer les données
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    epsilon <- rnorm(n, 0, 1)
    Y <- data$V1 * exp(2 * data$V2) + data$V3^2 + epsilon  # Génération de Y
    data$Y <- Y  # Ajout de Y aux données
    
    # Préparer les données pour mRMR
    mrmr_data <- mRMR.data(data = data)  # Convertir le data.frame pour mRMR
    
    # Appliquer mRMR.classic
    fit <- mRMR.classic(data = mrmr_data, target_indices = ncol(data), feature_count = d)
    
    # Vérifier les indices des caractéristiques sélectionnées
    selected_features <- as.numeric(fit@filters[[1]])  # Indices sélectionnés
    valeurs_communes <- intersect(selected_features, c(1, 2, 3))  # Intersect avec V1, V2, V3
    # Calculer le pourcentage
    pourcentage[i] <- (length(valeurs_communes) / d) * 100
  }
  return(pourcentage)
}

# Boucle pour plusieurs tailles de jeu de données
for (j in 1:10) {
  result_matrix3[j, ] <- Selected_features(25 * j, 1000, 3)
}
write.csv(result_matrix, file = "selected_mRMR.csv", row.names = FALSE)



#-----------------------------------------------------------------------
# On trace tous les résultats sur un même graphique 
#-----------------------------------------------------------------------
rm(list=ls())
res_lasso <- read.csv("models-selections/selected_LASSO.csv")
res_spam <- read.csv("models-selections/selected_spam.csv")
res_mrmr <- read.csv("models-selections/selected_mRMR.csv")
res_hsic <- read.csv("models-selections/selected_hsic.csv")


mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)

echantillon_points <- seq(25, 250, by = 25)

plot(echantillon_points, mean_row_hsic / 100, type = "l", 
     xlab = "Sample Size", 
     ylab = "Rate of correctly selected features",
     main = "Data2: Rate of correctly selected features by Sample Size",
     col = "red", lwd = 1)  # Rouge pour Lasso




# Ajouter les autres séries
lines(echantillon_points, mean_row_spam/100, col = "blue", lwd = 1)  # Bleu pour Spam
lines(echantillon_points, mean_row_mrmr/100, col = "darkgreen", lwd = 1) # Vert pour mRMR
lines(echantillon_points, mean_row_lasso/100, col = "purple", lwd = 1) # Violet pour HSIC

# Ajouter une légende pour identifier les couleurs
legend("topleft", legend = c("HSIC", "Spam", "mRMR", "Lasso"), 
       col = c("red", "blue", "darkgreen", "purple"), lwd = 1,cex=0.8)



