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

for (j in 1:10) {
  result_matrix3[j, ] <- Selected_features(25 * j, 1000, 3)
}
write.csv(result_matrix, file = "selected_mRMR.csv", row.names = FALSE)


