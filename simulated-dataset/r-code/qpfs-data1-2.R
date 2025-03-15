library(infotheo)
library(quadprog)
library(parallel)

rm(list=ls())
# Estimateur simplifié d'information mutuelle
estimate_mi_simple <- function(X, Y) {
  # Discrétisation des données continues pour estimation de l'information mutuelle
  X_discret <- discretize(X)
  Y_discret <- discretize(Y)
  
  # Estimation de l'information mutuelle
  mi <- mutinformation(X_discret, Y_discret)
  
  return(mi)
}

# Fonction de calcul parallélisé de la matrice de redondance
calculate_redundancy <- function(X, n) {
  # Initialisation de la matrice de redondance
  redundancy <- matrix(0, n, n)
  
  # Fonction pour calculer l'information mutuelle pour une paire donnée
  compute_mi <- function(i, j) {
    if (i != j) {
      mi <- estimate_mi_simple(X[, i], X[, j])
      return(list(i = i, j = j, mi = mi))
    }
    return(NULL)
  }
  
  # Utilisation de 'mclapply' pour paralléliser les calculs
  results <- mclapply(1:n, function(i) {
    sapply(i:n, function(j) compute_mi(i, j), simplify = FALSE)
  }, mc.cores = detectCores())  # Utilise tous les cœurs disponibles
  
  # Remplir la matrice de redondance avec les résultats
  for (result_list in results) {
    for (result in result_list) {
      if (!is.null(result)) {
        i <- result$i
        j <- result$j
        mi <- result$mi
        redundancy[i, j] <- mi
        redundancy[j, i] <- mi  # Symétrie de la matrice
      }
    }
  }
  
  return(redundancy)
}

# Fonction optimisée de sélection de variables avec QPFS
qpfs_selection <- function(X, y, num_features, lambda = 10) {
  n <- ncol(X)  # Nombre de variables
  
  # Étape 1 : Calcul de la pertinence (Information mutuelle entre X et y)
  relevance <- apply(X, 2, function(x) estimate_mi_simple(x, y))
  
  # Étape 2 : Calcul de la redondance (Information mutuelle entre variables)
  redundancy <- calculate_redundancy(X, n)  # Calcul parallélisé de la redondance
  
  # Étape 3 : Ajout de régularisation pour garantir la positive définition
  Dmat <- redundancy + lambda * diag(n)  # Augmenter lambda si nécessaire
  
  # Étape 4 : Problème d'optimisation quadratique
  dvec <- relevance
  Amat <- cbind(rep(1, n), diag(n))  # Contraintes alpha >= 0 et somme(alpha) = 1
  bvec <- c(1, rep(0, n))
  
  # Résolution de l'optimisation quadratique avec gestion des erreurs
  sol <- tryCatch({
    solve.QP(Dmat, dvec, Amat, bvec, meq = 1)  # Résolution
  }, error = function(e) {
    message("Erreur lors de l'optimisation : ", e$message)
    return(NULL)
  })
  
  if (is.null(sol)) {
    stop("L'optimisation quadratique a échoué.")
  }
  
  alpha <- abs(sol$solution)  # Récupération des poids
  selected_features <- order(alpha, decreasing = TRUE)[1:num_features]  # Meilleures variables
  
  return(selected_features)
}


############################################################################################
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
data <- as.data.frame(matrix(rnorm(n*p,0,1),n,p))
colnames(data) <- paste0("X_", 1:p)
epsilon <- rnorm(n,0,1)
Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
res <- qpfs_selection(data,Y,4)
############################################################################################

# data1
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(0803)  # Pour reproduire les résultats

rep <- 15
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    selected_features <- qpfs_selection(data,Y,d)
    print(selected_features)
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_qpfs1 <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_qpfs1[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}
rmean1 <- rowMeans(selected_qpfs1)

echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean1/100,type="l",
     ylim = range(0,1))
write.csv(selected_qpfs1, file = "selected_qpfs_data1.csv", row.names = FALSE)

# data2
set.seed(0803)  # Pour reproduire les résultats
rep <- 10
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon
    selected_features <- qpfs_selection(data,Y,d)
    print(selected_features)
    val_com <- intersect(selected_features,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_qpfs2 <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_qpfs2[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,3)
}
rmean2 <- rowMeans(selected_qpfs2)

echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean2/100,type="l",
     ylim = range(0,1))
write.csv(selected_qpfs2, file = "selected_qpfs_data2.csv", row.names = FALSE)


