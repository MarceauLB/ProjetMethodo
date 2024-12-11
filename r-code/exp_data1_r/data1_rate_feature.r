setwd("~/00_Ensai/projet-methodo/ProjetMethodo/r-code/exp_data1_r/")

####################################################################
# data 1 
####################################################################

rm(list=ls())

n <- 250  # Nombre de lignes (observations)
p <- 256  # Nombre de colonnes (variables)

# Génération du dataframe avec 256 colonnes, chaque colonne suit une loi normale N(0, 1)

set.seed(123)  # Pour reproduire les résultats

data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
colnames(data) <- paste0("X_", 1:p)

head(data)

epsilon <- rnorm(n,0,1)

Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon


train_index <- sample(1:n, size = 0.8 * n)  # 70% des données pour l'entraînement
X_train <- as.matrix(data[train_index, ])
Y_train <- Y[train_index]
X_test <- as.matrix(data[-train_index, ])
Y_test <- Y[-train_index]

# Lasso 
library(glmnet)
X <- as.matrix(data)
lasso_model <- cv.glmnet(X_train, Y_train, alpha = 1, family = "gaussian",nfolds = 5)
print(lasso_model)
best_lambda <- lasso_model$lambda.min
best_lambda

coef(lasso_model, s = best_lambda)

predictions <- predict(lasso_model, s = best_lambda, newx = X_test)

head(predictions)
sum((Y_test - predictions)**2)

# Plot des coefficients en fonction de log(lambda)
plot(lasso_model$glmnet.fit, xvar = "lambda", label = TRUE)



####################################################################
# HSIC 
####################################################################

rm(list=ls())
# install.packages('GSelection')
library(GSelection)

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
  # print(a$hsic_selected_feature_index)
  val_com <- intersect(a$hsic_selected_feature_index,c(1,2,3,4))
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

write.csv(selected_feature_n, file = "selected_hsic.csv", row.names = FALSE)



####################################################################
# Spam Selections 
####################################################################

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

####################################################################
# mRMR Selections 
####################################################################

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


####################################################################
# Lasso Selections 
####################################################################
rm(list=ls())

library(glmnet)

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
    lasso_fit <- glmnet(X,Y,alpha=1) # alpha = 1 lasso et pas ridge
  
    # selection de variables (validation croissé lambda)
    cv_fit <- cv.glmnet(X, Y, alpha = 1)
    best_lambda <- cv_fit$lambda.min
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  # Exclure l'intercept
    
    # Obtenir les indices des 4 plus grandes valeurs absolues des coefficients
    selected_features <- order(abs(coefficients), decreasing = TRUE)[1:d]
    
    val_com <- intersect(selected_features,c(1,2,3,4))
    list_pourcentage[i] <- (length(val_com)/4)*100
  }
  return((list_pourcentage))
}

selected_features_lasso <- matrix(0,nrow = 16,ncol = 30)
seq(25, 250, by = 15)
length(seq(25, 250, by = 15))

for(taille_echantillon in 1:16){
  selected_features_lasso[taille_echantillon,] <- prc_selected_features(10+taille_echantillon*25, 256,4)
}

write.csv(selected_features_lasso, file = "selected_lasso.csv", row.names = FALSE)


####################################################################
# On trace tous les résultats sur un même graphique 
####################################################################
rm(list=ls())
res_lasso <- read.csv("selected_lasso.csv")
res_spam <- read.csv("selected_spam.csv")
res_mrmr <- read.csv("selected_mrmr.csv")
res_hsic <- read.csv("selected_hsic.csv")


mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)

echantillon_points <- seq(25, 250, by = 15)

plot(echantillon_points, mean_row_hsic / 100, type = "l", 
     xlab = "Taille de l'échantillon", 
     ylab = "Moyenne des lignes",
     main = "Moyenne des lignes par taille d'échantillon",
     col = "red", lwd = 1)  # Rouge pour Lasso

# Ajouter les autres séries
lines(echantillon_points, mean_row_spam/100, col = "blue", lwd = 1)  # Bleu pour Spam
lines(echantillon_points, mean_row_mrmr/100, col = "darkgreen", lwd = 1) # Vert pour mRMR
lines(echantillon_points, mean_row_lasso/100, col = "purple", lwd = 1) # Violet pour HSIC

# Ajouter une légende pour identifier les couleurs
legend("topleft", legend = c("HSIC", "Spam", "mRMR", "Lasso"), 
       col = c("red", "blue", "darkgreen", "purple"), lwd = 1,cex=0.8)

