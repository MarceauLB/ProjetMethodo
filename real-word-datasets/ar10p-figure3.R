rm(list=ls())

library(GSelection)
library(praznik)
library(glmnet)

X <- read.csv("dataset_csv/ar10p.csv")
n <- dim(X)[1]
p <- dim(X)[2]-1

Y <- X[,-(1:p)]
X <- X[,-(p+1)]

MRMR(ra)


indices <- sample(1:n,size = round(0.8*n),replace=FALSE)
X_train <- X[indices,]
Y_train <- Y[indices]
X_test <- X[-indices,]
Y_test <- Y[-indices]

res_hsic <- feature.selection(X_train,Y_train,d=50)
res_hsic$coefficient.hsic
res_hsic$hsic_selected_feature_index


resmrmr <- MRMR(X_train, Y_train,50)
restable_mrmr <- as.data.frame(resmrmr)
restable_mrmr

resmrmr20 <- MRMR(X_train, Y_train,20)

?glmnet
resglmnet <- glmnet(X_train, Y_train,alpha=1)
cv_fit <- cv.glmnet(as.matrix(X_train), Y_train,alpha=1)
lambda_opt <- cv_fit$lambda.min
coefficients <- as.numeric(coef(cv_fit, s = "lambda.min"))[-1]  
selected_features <- order(abs(coefficients), decreasing = TRUE)[1:50]


