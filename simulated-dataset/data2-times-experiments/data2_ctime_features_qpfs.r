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




n <- 100
d <- 1000
set.seed(123)

matrix_res <- matrix(0, ncol = 30, nrow=10, byrow = TRUE)

time_execution <- function(d){
  times_30 <- numeric(30)
  
  for(i in 1:3){
    X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
    epsilon <- rnorm(n,0,1)
    Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon
    
    
    
    res_time <- system.time(cKTA(X,Y))
    
    times_30[i] <- res_time[["elapsed"]]
  }
  return(times_30)
}

for (nb_feature_generated in seq(100, 1000, 100)){
  print(nb_feature_generated)
  list_time <- time_execution(nb_feature_generated)
  matrix_res[nb_feature_generated/100,] <- list_time
}

write.csv(matrix_res,file="qpfs_times_features",row.names = FALSE)
