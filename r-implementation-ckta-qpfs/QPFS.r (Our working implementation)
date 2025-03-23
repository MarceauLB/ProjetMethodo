```{r}
install.packages('ks')
```


#Code non optimisé
#------------------------------------------------------------
#-------------------------NON-OPTI---------------------------
#------------------------------------------------------------

```{r}
estimate_mi_simple <- function(X, Y) {
  # Discrétisation des données continues pour estimation de l'information mutuelle
  X_discret <- discretize(X)
  Y_discret <- discretize(Y)
  
  # Estimation de l'information mutuelle
  mi <- mutinformation(X_discret, Y_discret)
  
  return(mi)
}

# Fonction de sélection de variables avec QPFS
qpfs_selection <- function(X, y, num_features) {
  n <- ncol(X)  # Nombre de variables
  
  relevance <- sapply(1:n, function(i) estimate_mi_simple(X[, i], y))
  print('etape1')

  redundancy <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in i:n) {
      if (i != j) {
        mi <- estimate_mi_simple(X[, i], X[, j])
        redundancy[i, j] <- mi
        redundancy[j, i] <- mi  # Symétrie de la matrice
      }
    }
  }
  print('etape2')

  Dmat <- redundancy + diag(n) * 0.1  # Ajout d'une régularisation
  
  dvec <- relevance
  Amat <- cbind(rep(1, n), diag(n))  # Contraintes alpha >= 0 et somme(alpha) = 1
  bvec <- c(1, rep(0, n))
  print('etape3')
  
  sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)  # Résolution
  
  alpha <- abs(sol$solution)  # Récupération des poids
  selected_features <- order(alpha, decreasing = TRUE)[1:num_features]  # Meilleures variables

  return(selected_features)
}
```

#Code optimisé
#------------------------------------------------------------
#----------------------------OPTI----------------------------
#------------------------------------------------------------

```{r}
library(infotheo)
library(quadprog)
library(parallel)

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
qpfs_selection <- function(X, y, num_features, lambda = 0.1) {
  n <- ncol(X)  # Nombre de variables
  
  relevance <- apply(X, 2, function(x) estimate_mi_simple(x, y))
  
  redundancy <- calculate_redundancy(X, n)  # Calcul parallélisé de la redondance
  
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
```



```{r}
set.seed(12)
n <- 250 # Nombre d'observations
p <- 256   # Nombre de variables

data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
colnames(data) <- paste0("X_", 1:p)

eps <- rnorm(n, 0, 1)
Y <- -2*sin(2*data$X_1) + (data$X_2)^2 + data$X_3 + exp(-data$X_4) + eps


result <- qpfs_selection(data, Y, num_features = 4)
print(result)
```
