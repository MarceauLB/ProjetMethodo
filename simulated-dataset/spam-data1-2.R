#-------------------------------------------------------------------------------
### Projet méthodologique - SpAM
#-------------------------------------------------------------------------------

rm(list=ls())
library(SAM)
set.seed(1234)

do_cv <- function(X,Y,K_fold=5){
  n <- dim(X)[1]
  p <- dim(X)[2]
  lambda_seq <- numeric(K_fold)
  
  # Fixed grid of lambdas on the whole sample
  res_samQL <- samQL(X, Y)
  grid_lamb <- res_samQL$lambda
  
  # Iterations (using the previous grid of lambdas)
  for(k in 1:K_fold){
    X_indice_train <- sample(c(1:n),size = round(0.8*n),replace = FALSE)
    X_train <- X[X_indice_train,]
    Y_train <- Y[X_indice_train]
    
    X_val <- X[-X_indice_train,]
    Y_val <- Y[-X_indice_train]
    
    res_samQL <- samQL(X_train, Y_train, lambda = grid_lamb)
    prediction <- predict(res_samQL,X_val)
    y_pred_val <- prediction$values
    
    best_lambda_index <- which.min(colMeans((y_pred_val - Y_val)**2))
    lambda_seq[k] <- grid_lamb[best_lambda_index]
  }
  lambda_opt <- mean(lambda_seq)
  return(lambda_opt)
}

rep <- 30
prc_selected_features <- function(n,p,d){
  list_pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- -2*sin(2*data$X_1) + (data$X_2)**2 + data$X_3 + exp(-data$X_4) + epsilon
    X <- as.matrix(data)
    
    lambda_opt <- do_cv(X,Y,K_fold=5)
    res_spam <- samQL(X,Y,lambda = lambda_opt)
    coefs_spam_positifs = res_spam[which(res_spam$func_norm > 0)]
    nb_coefs_spam_positifs = length(coefs_spam_positifs)
    
    if (nb_coefs_spam_positifs == 0){
      list_pourcentage[i] = 0
    }
    
    spam_index <- order(res_spam$func_norm,decreasing = TRUE)[1:min(nb_coefs_spam_positifs,d)]
    
    val_com <- intersect(spam_index,c(1:d))
    list_pourcentage[i] <- (length(val_com)/d)*100
  }
  return((list_pourcentage))
}

selected_spam_1 <- matrix(0,nrow = 10,ncol = rep)


for(sample_size in 2:10){
  print(sample_size*25)
  selected_spam_1[sample_size,] <- prc_selected_features(sample_size*25, 256,4)
}
rmean <- rowMeans(selected_spam_1)

echant <- seq(25,250,25)
plot(echant[2:10], rmean[2:10]/100,type="l",ylim = c(0,1), 
     main = "Correctly selected features with SpAM method on an additive model (data1)",
     xlab = "Sample size", ylab = "Rate of correctly selected features")

write.csv(selected_spam_1, file = "selected_spam_data1.csv", row.names = FALSE)





#-----------------------------------------
# data2 
#-----------------------------------------
set.seed(123)
rep <- 30
Selected_features <- function(n,p,d){
  pourcentage <- numeric(rep)
  for(i in 1:rep){
    data <- as.data.frame(matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p))
    colnames(data) <- paste0("X_", 1:p)
    epsilon <- rnorm(n,0,1)
    Y <- data$X_1*exp(2*data$X_2) + data$X_3**2 + epsilon
    X <- as.matrix(data)
    
    
    lambda_opt <- do_cv(X,Y,K_fold=5)
    res_spam <- samQL(X,Y,lambda = lambda_opt)
    coefs_spam_positifs = res_spam[which(res_spam$func_norm > 0)]
    nb_coefs_spam_positifs = length(coefs_spam_positifs)
    
    if (nb_coefs_spam_positifs == 0){
      pourcentage[i] = 0
    }
    spam_index <- order(res_spam$func_norm,decreasing = TRUE)[1:d]
    
    valeurs_communes <- intersect(spam_index, c(1:min(nb_coefs_spam_positifs,d)))
    pourcentage[i] <- (length(valeurs_communes)/d) * 100
  }
  return(pourcentage)
}

selected_spam_2 <- matrix(nrow = 10, ncol = rep)

for(j in 2:10){
  print(j*25)
  selected_spam_2[j, ] <- Selected_features(25*j,1000,3)
}
rmean2 <- rowMeans(selected_spam_2)

echant <- seq(25,250,25)
length(echant)
plot(echant[2:10], rmean2[2:10]/100,type="l",ylim = c(0,1),  
     main = "Correctly selected features with SpAM method on a non-additive model",
     xlab = "Sample size", ylab = "Rate of correctly selected features")


write.csv(selected_spam_2, file = "selected_spam_data2.csv", row.names = FALSE)