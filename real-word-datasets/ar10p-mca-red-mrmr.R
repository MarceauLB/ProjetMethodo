rm(list=ls())
set.seed(12345)

library(praznik)
source("red_score.R")

X <- read.csv("ar10p.csv")
n <- dim(X)[1]
p <- dim(X)[2]-1
Y <- X[,-(1:p)]
X <- X[,-(p+1)]
X <- as.data.frame(scale(X,center = TRUE,scale = TRUE))

# Work done for mrmr 
#--------------------------------------------------------------------------
rep <- 50
features_index <- seq(10,50,10)
top_m <- features_index[length(features_index)]
MCAcc_mrmr <- matrix(0,nrow = length(features_index),ncol = rep)
RED_table50_mrmr <- matrix(0,ncol=rep,nrow=1)

for(i in 1:rep){
  x_index <- sample(1:n,size = round(n*0.8),replace = FALSE)
  X_train <- X[x_index,]
  Y_train <- Y[x_index]
  X_test <- X[-x_index,]
  Y_test <- Y[-x_index]
  
  model_mrmr <- MRMR(X_train, Y_train,top_m) 
  df_model_mrmr <- as.data.frame(model_mrmr)
  ModelFeatureIndex <- df_model_mrmr$selection
  for(m_index in seq_along(features_index)){
    nb_features <- features_index[m_index]
    x_features <- ModelFeatureIndex[1:nb_features]
    model <- gausspr(X_train[,c(x_features)], as.factor(Y_train),
                     kernel = "rbfdot",
                     kpar = list(sigma = 0.1))
    y_pred <- predict(model, X_test[,c(x_features)])
    MCAcc_mrmr[m_index,i] <- sum(y_pred==Y_test)/length(Y_test)
    
    if(nb_features == top_m){
      RED_table50_mrmr[1,i] <- res_score(x_features, nb_features, X_train)
    }
  }
}

rmean_mrmr <- rowMeans(MCAcc_mrmr)
mean(MCAcc_mrmr[5,])
sqrt(var(MCAcc_mrmr[5,]))
sqrt(var(RED_table50_mrmr[1,]))
plot(features_index,rmean_mrmr,type="l",col="cyan",ylim=c(0,1))

write(MCAcc_mrmr,"ar10p_mca_MRMR.csv",append = FALSE)
write(RED_table50_mrmr,"ar10p_red50_MRMR.csv",append = FALSE)
