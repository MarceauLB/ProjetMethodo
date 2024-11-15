rm(list=ls())
# changement marceau 
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


# HSIC 
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

echantillon_points <- seq(25, 250, by = 15)
moyenne_lignes <- rowMeans(selected_feature_n)

plot(echantillon_points, moyenne_lignes/100, type = "l", 
     xlab = "Taille de l'échantillon", 
     ylab = "Moyenne des lignes",
     main = "Moyenne des lignes par taille d'échantillon")



#SpAM Selections 
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

echantillon_points <- seq(25, 250, by = 15)
moyenne_lignes <- rowMeans(selected_feature_n)

plot(echantillon_points, moyenne_lignes/100, type = "l", 
     xlab = "Taille de l'échantillon", 
     ylab = "Moyenne des lignes",
     main = "Moyenne des lignes par taille d'échantillon",ylim = c(0,1))

