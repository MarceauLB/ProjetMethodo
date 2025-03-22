rm(list=ls())
set.seed(12345)

library(GSelection)

X <- read.csv("ar10p.csv")
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- X[,-(1:p)]
X <- X[,-(p+1)]
X <- as.data.frame(scale(X,center = TRUE,scale = TRUE))

rep <- 50
features_index <- seq(10,50,10)
top_m <- features_index[length(features_index)]
MCAcc <- matrix(nrow = length(features_index),ncol = rep)
MCAcc
for(i in 1:rep){
  print(i)
  x_index <- sample(1:n,size = round(n*0.8),replace = FALSE)
  X_train <- X[x_index,]
  Y_train <- Y[x_index]
  X_test <- X[-x_index,]
  Y_test <- Y[-x_index]
  
  a <- feature.selection(X_train,Y_train,top_m)
  
  ModelFeatureIndex <- a$hsic_selected_feature_index
  for(m_index in seq_along(features_index)){
    nb_features <- features_index[m_index]
    x_features <- ModelFeatureIndex[1:nb_features]
    model <- gausspr(X_train[,c(x_features)], as.factor(Y_train),
                     kernel = "rbfdot",
                     kpar = list(sigma = 0.1))
    y_pred <- predict(model, X_test[,c(x_features)])
    MCAcc[m_index,i] <- sum(y_pred==Y_test)/length(Y_test)
  }
}

rmean <- rowMeans(MCAcc)
mean(MCAcc[5,])
sqrt(var(MCAcc[5,]))

plot(features_index,rmean,type="l",col="black",ylim=c(0.5,1),lwd=2,lty=2)

write.csv(MCAcc,file="hsic_res_gaussian_fromR.csv",row.names=FALSE)
