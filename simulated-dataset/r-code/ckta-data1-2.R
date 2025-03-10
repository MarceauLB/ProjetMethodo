#-----------------------------------------------------------
#------------------------NON-OPTI---------------------------
#-----------------------------------------------------------
compute_kernel_matrix <- function(X, sigma = 1) {
  # Calcul plus rapide de la distance au carré via une méthode de produit matriciel
  n <- nrow(X)
  XX <- rowSums(X^2)
  D <- outer(XX, XX, FUN = "+") - 2 * X %*% t(X)
  D[D < 0] <- 0  # En cas de petites erreurs numériques
  # Application du noyau gaussien
  K <- exp(-D / (2 * sigma^2))
  return(K)
}

# Fonction optimisée de calcul de HSIC
compute_HSIC <- function(K, L) {
  n <- nrow(K)
  H <- diag(n) - matrix(1, n, n) / n  # Matrice de centrage Γ
  # Calcul du produit matriciel de manière plus rapide
  KH = H %*% K %*% H
  LH = H %*% L %*% H
  return(sum(KH * LH))  # Utilisation de * pour le produit élément par élément
}

# Fonction principale pour cKTA optimisée
cKTA <- function(X, y, sigma_X = 1, sigma_Y = 1, epsilon = 1e-6) {
  d <- ncol(X)  # Nombre de variables
  n <- nrow(X)  # Nombre d'échantillons
  
  # Matrice de Gram de Y
  L <- compute_kernel_matrix(matrix(y, ncol = 1), sigma_Y)
  
  # Calcul des matrices de noyau pour chaque variable X de manière plus optimisée
  K_list <- lapply(1:d, function(s) compute_kernel_matrix(matrix(X[, s], ncol = 1), sigma_X))
  
  # Calcul des HSIC_XY plus rapidement en parallèle
  HSIC_XY <- sapply(K_list, function(K_s) compute_HSIC(K_s, L))
  
  # Calcul des HSIC_XX (matrice d'interactions entre les différentes dimensions de X)
  HSIC_XX <- matrix(0, nrow = d, ncol = d)
  for (s in 1:d) {
    for (t in s:d) {
      HSIC_XX[s, t] <- compute_HSIC(K_list[[s]], K_list[[t]])
      if (s != t) {
        HSIC_XX[t, s] <- HSIC_XX[s, t]  # Matrice symétrique
      }
    }
  }
  
  # Ajout d'une régularisation pour assurer l'inversibilité
  HSIC_XX <- HSIC_XX + epsilon * diag(d)
  
  # Formulation du problème d'optimisation quadratique (QP)
  Dmat <- HSIC_XX
  dvec <- HSIC_XY
  Amat <- diag(d)  # Contraintes α ≥ 0
  bvec <- rep(0, d)
  
  # Résolution de l'optimisation quadratique
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 0)
  alpha <- abs(sol$solution)
  
  return(list(alpha = alpha, HSIC_XY = HSIC_XY, HSIC_XX = HSIC_XX))
}

#-----------------------------------------------------------
#--------------------------OPTI-----------------------------
#-----------------------------------------------------------

library(quadprog)
library(parallel)

# Fonction optimisée de calcul de la matrice de noyau
compute_kernel_matrix <- function(X, sigma = 1) {
  n <- nrow(X)
  XX <- rowSums(X^2)  # Calcul de la somme des carrés de chaque ligne
  D <- outer(XX, XX, FUN = "+") - 2 * X %*% t(X)
  D[D < 0] <- 0  # Corriger les erreurs numériques dues aux petites valeurs négatives
  K <- exp(-D / (2 * sigma^2))  # Noyau gaussien
  return(K)
}

# Fonction optimisée de calcul de HSIC
compute_HSIC <- function(K, L) {
  n <- nrow(K)
  H <- diag(n) - matrix(1, n, n) / n  # Matrice de centrage Γ
  KH = H %*% K %*% H
  LH = H %*% L %*% H
  return(sum(KH * LH))  # Produit élément par élément
}

# Fonction principale optimisée pour cKTA
cKTA <- function(X, y, sigma_X = 1, sigma_Y = 1, epsilon = 1e-6) {
  d <- ncol(X)  # Nombre de variables
  n <- nrow(X)  # Nombre d'échantillons
  
  # Matrice de Gram de y
  L <- compute_kernel_matrix(matrix(y, ncol = 1), sigma_Y)
  
  # Calcul parallèle des noyaux pour chaque variable de X
  K_list <- mclapply(1:d, function(s) compute_kernel_matrix(matrix(X[, s], ncol = 1), sigma_X), mc.cores = detectCores())
  
  # Calcul parallèle des HSIC_XY
  HSIC_XY <- unlist(mclapply(K_list, function(K_s) compute_HSIC(K_s, L), mc.cores = detectCores()))
  
  # Calcul parallèle de la matrice HSIC_XX (partie supérieure symétrique)
  HSIC_XX <- matrix(0, nrow = d, ncol = d)
  
  # Utilisation de mclapply pour calculer la matrice HSIC_XX de manière efficace
  result_HSIC_XX <- mclapply(1:d, function(s) {
    res <- sapply(s:d, function(t) {
      HSIC_val <- compute_HSIC(K_list[[s]], K_list[[t]])
      return(c(s, t, HSIC_val))  # Retourner sous forme de vecteur avec [s, t, HSIC_val]
    })
    return(res)
  }, mc.cores = detectCores())
  
  
  # Remplir la matrice HSIC_XX à partir des résultats parallélisés
  for (i in 1:length(result_HSIC_XX)) {
    for (j in 1:ncol(result_HSIC_XX[[i]])) {
      s <- result_HSIC_XX[[i]][1, j]  # Premier élément de chaque vecteur
      t <- result_HSIC_XX[[i]][2, j]  # Deuxième élément de chaque vecteur
      HSIC_val <- result_HSIC_XX[[i]][3, j]  # Troisième élément de chaque vecteur
      
      if (s <= d && t <= d) {  # Vérification que les indices sont dans les limites
        HSIC_XX[s, t] <- HSIC_val
        HSIC_XX[t, s] <- HSIC_val  # Matrice symétrique
      }
    }
  }
  
  # Ajouter une régularisation pour éviter l'inversibilité de la matrice
  HSIC_XX <- HSIC_XX + epsilon * diag(d)
  
  # Problème d'optimisation quadratique
  Dmat <- HSIC_XX
  dvec <- HSIC_XY
  Amat <- diag(d)  # Contraintes α ≥ 0
  bvec <- rep(0, d)
  
  # Résolution de l'optimisation quadratique
  sol <- tryCatch({
    solve.QP(Dmat, dvec, Amat, bvec, meq = 0)
  }, error = function(e) {
    message("Erreur lors de l'optimisation : ", e$message)
    return(NULL)
  })
  
  if (is.null(sol)) {
    stop("L'optimisation quadratique a échoué.")
  }
  
  alpha <- abs(sol$solution)
  
  return(list(alpha = alpha, HSIC_XY = HSIC_XY, HSIC_XX = HSIC_XX))
}


#-----------------------------------------------------------
#----------------------EXPERIENCES--------------------------
#-----------------------------------------------------------

n <- 200
p <- 1000

data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
eps <- rnorm(n,0,1)
colnames(data) <- paste0("X_", 1:p)
head(data)

Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + eps


result <- cKTA(data, Y)

# Affichage des résultats
top_indices <- order(result$alpha, decreasing = TRUE)[1:4]

# Afficher les indices des 4 plus grandes valeurs de alpha
print(top_indices)




# data1
n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)
set.seed(0803)  # Pour reproduire les résultats

rep <- 6
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    ckta_res <- cKTA(data,Y)
    top_indices <- order(ckta_res$alpha, decreasing = TRUE)[1:4]
    val_com <- intersect(top_indices,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feature_ckta <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_feature_ckta[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,4)
}
rmean <- rowMeans(selected_feature_ckta)

echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,type="l",
     ylim = range(0,1))

write.csv(selected_feature_ckta, file = "selected_ckta_data1.csv", row.names = FALSE)


#------------------------------------------------------------------------------
# data2
set.seed(0803)  # Pour reproduire les résultats
rep <- 6
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon
    ckta_res <- cKTA(data,Y)
    top_indices <- order(ckta_res$alpha, decreasing = TRUE)[1:d]
    val_com <- intersect(top_indices,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_feature_ckta2 <- matrix(nrow = 10,ncol = rep)

for(taille_echantillon in 1:10){
  print(taille_echantillon*25)
  selected_feature_ckta2[taille_echantillon,] <- prc_selected_features(taille_echantillon*25, 256,3)
}
rmean <- rowMeans(selected_feature_ckta2)

echantillon_points <- seq(25, 250, by = 25)
plot(echantillon_points,rmean/100,type="l",
     ylim = range(0,1))
write.csv(selected_feature_ckta2, file = "selected_ckta_data1.csv", row.names = FALSE)

