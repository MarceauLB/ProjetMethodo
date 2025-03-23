rm(list=ls())
library(SAM)
library(combinat)
library(caret)
library(kernlab)
source("red_score.R")
set.seed(2303)

X <- read.csv("ar10p.csv")
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- as.factor(X[,-(1:p)])
X <- X[,-(p+1)]
X <- as.data.frame(scale(X,center = TRUE,scale = TRUE))
X <- as.matrix(X)

get_lambda_optimal <- function(X,Y_coded,K_fold,lambda_grid){
  n <- dim(X)[1]
  p <- dim(X)[2]
  lambda_seq <- numeric(K_fold)
  train_idx <- createDataPartition(Y_coded, p = 0.65, list = FALSE,times=K_fold)
  
  # Iterations (using the previous grid of lambdas)
  for(k in 1:K_fold){
    X_indice_train <- train_idx[,1]
    X_train <- X[X_indice_train,]
    Y_train <- Y_coded[X_indice_train]
    
    X_val <- X[-X_indice_train,]
    Y_val <- Y_coded[-X_indice_train]
    
    # Attempt to fit model, if it fails, return a random lambda
    sam_boost <- tryCatch({
      samLL(X_train, Y_train, lambda = lambda_grid)
    }, error = function(e) {
      return(NULL)  # Indicate failure
    })
    
    if (is.null(sam_boost)) {
      lambda_seq[k] <- sample(lambda_grid, 1)  # Random lambda from the grid
      next  # Skip to next iteration
    }
    
    # Attempt to make predictions, if it fails, return a random lambda
    prediction <- tryCatch({
      predict(sam_boost, X_val)
    }, error = function(e) {
      return(NULL)  # Indicate failure
    })
    
    if (is.null(prediction)) {
      lambda_seq[k] <- sample(lambda_grid, 1)  # Random lambda from the grid
      next  # Skip to next iteration
    }
    
    y_pred_val <- prediction$labels
    
    best_lambda_index <- which.min(colMeans(y_pred_val != Y_val))
    lambda_seq[k] <- lambda_grid[best_lambda_index]
  }
  lambda_opt <- mean(lambda_seq)
  return(lambda_opt)
}

index_feature_OVA <- function(X,Y){
  redundant_index <- matrix(nrow =10,ncol = 50)
  
  classes <- unique(Y)  
  classes <- as.numeric(classes)
  
  for(classe in classes){
    # sous jeu de données pour One vs All
    Y_coded <- ifelse(Y==classe,1,0)
    
    # Try initial samLL model
    res_sam <- tryCatch({
      samLL(X, Y_coded)
    }, error = function(e) {
      return(NULL)
    })
    
    # If res_sam fails, skip to the next class
    if (is.null(res_sam)) {
      print(paste("Skipping class", classe, "due to samLL error"))
      next
    }
    
    lambda_grid <- res_sam$lambda
    
    get_best_lambda <- get_lambda_optimal(X,Y_coded,K_fold = 2,lambda_grid)
    
    final_SAM <- tryCatch({
      samLL(X, Y_coded, lambda = get_best_lambda)
    }, error = function(e) {
      return(NULL)
    })
    
    # If final_SAM fails, skip this iteration
    if (is.null(final_SAM)) {
      print(paste("Skipping final model for class", classe))
      next
    }
    
    index_selected <- order(final_SAM$func_norm,decreasing = TRUE)[1:50]
    
    # stocker les index des lambdas 
    redundant_index[classe,] <- index_selected
  }
  # 50 indices les plus redondants parmi toutes les lignes 
  total_selected <- as.vector(redundant_index)
  top_indices <- as.numeric(names(sort(table(total_selected),decreasing = TRUE)[1:50]))
  return(top_indices)
}

# top_indices <- index_feature_OVA(X,Y)


rep <- 15
features_index <- seq(10,50,10)
top_m <- features_index[length(features_index)]
MCAcc <- matrix(0,nrow = length(features_index),ncol = rep)
RED_table50 <- matrix(0,ncol=rep,nrow=1)

for(i in 1:rep){
  print(i)
  x_index <- sample(1:n,size = round(n*0.95),replace = FALSE)
  X_train <- X[x_index,]
  Y_train <- Y[x_index]
  X_test <- X[-x_index,]
  Y_test <- Y[-x_index]
  ModelFeatureIndex <- index_feature_OVA(X_train,Y_train)
  
  for(m_index in seq_along(features_index)){
    nb_features <- features_index[m_index]
    x_features <- ModelFeatureIndex[1:nb_features]
    model <- gausspr(X_train[,c(x_features)], as.factor(Y_train),
                     kernel = "rbfdot",
                     kpar = list(sigma = 0.1))
    y_pred <- predict(model, X_test[,c(x_features)])
    MCAcc[m_index,i] <- sum(y_pred==Y_test)/length(Y_test)
    
    if(nb_features == top_m){
      RED_table50[1,i] <- res_score(x_features, nb_features, X_train)
    }
  }
}

rmean <- rowMeans(MCAcc)
mean(MCAcc[5,])
sqrt(var(MCAcc[5,]))
rowMeans(RED_table50)
sqrt(var(RED_table50[1,]))


plot(features_index,rmean,type="l",col="black",ylim=c(0,1))


write.csv(MCAcc,file="ar10p_spam_mca.csv",row.names=FALSE)
write.csv(RED_table50,file="ar10p_spam_red50.csv",row.names=FALSE)


