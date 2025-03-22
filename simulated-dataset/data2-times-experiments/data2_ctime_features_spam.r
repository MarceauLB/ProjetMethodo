rm(list=ls())
library(SAM)

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

n <- 100
d <- 1000
set.seed(123)
rep <- 15

matrix_res <- matrix(0, ncol = rep, nrow=10, byrow = TRUE)

time_execution <- function(d){
  times_30 <- numeric(rep)
  
  for(i in 1:rep){
    X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
    epsilon <- rnorm(n,0,1)
    Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon
    X <- as.matrix(X)
    res_time <- system.time({
      lambda_opt <- do_cv(X,Y,K_fold=5)
      res_spam <- samQL(X,Y)
      res_spam$lambda
    })
    
    times_30[i] <- res_time[["elapsed"]]
  }
  return(times_30)
}

for (nb_feature_generated in seq(100, 1000, 100)){
  print(nb_feature_generated)
  list_time <- time_execution(nb_feature_generated)
  matrix_res[nb_feature_generated/100,] <- list_time
}

write.csv(matrix_res,file="spam_time_features.csv",row.names = FALSE)

rm <- rowMeans(matrix_res)
features <- seq(100,1000,100)
plot(features,rm,type="l")



