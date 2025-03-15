rm(list=ls())
set.seed(12345)

library(GSelection)
library(praznik)
library(glmnet)
library(kernlab)
source("red_score.R")

X <- read.csv("ar10p.csv")
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- as.factor(X[,-(1:p)])
X <- X[,-(p+1)]
X <- as.data.frame(scale(X,center = TRUE,scale = TRUE))
X <- as.matrix(X)

rep <- 50
features_index <- seq(10,50,10)
top_m <- features_index[length(features_index)]
MCAcc <- matrix(0,nrow = length(features_index),ncol = rep)
RED_table50 <- matrix(0,ncol=rep,nrow=1)

?glmnet
for(i in 1:rep){
  x_index <- sample(1:n,size = round(n*0.8),replace = FALSE)
  X_train <- X[x_index,]
  Y_train <- Y[x_index]
  X_test <- X[-x_index,]
  Y_test <- Y[-x_index]
  
  cv_fit <- cv.glmnet(X_train, Y_train,family="multinomial", alpha = 1)
  best_lambda <- cv_fit$lambda.min
  coeff_list <- coef(cv_fit)
  coefficients_matrix <- do.call(cbind, lapply(coeff_list, function(mat) as.numeric(mat[-1, ])))
  variable_importance <- rowSums(abs(coefficients_matrix))
  ModelFeatureIndex <- order(variable_importance, decreasing = TRUE)[1:top_m]
  
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
sqrt(var(RED_table50_mrmr[1,]))
plot(features_index,rmean,type="l",col="blue",ylim=c(0,1))
