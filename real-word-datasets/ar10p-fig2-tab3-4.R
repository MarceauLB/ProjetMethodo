# tableau figure 2, table 3 et table 4 de l'article 
rm(list=ls())
set.seed(123)

library(calibrateBinary)
library(GSelection)
source("../red_score.R")

ar10p <- read.csv("ar10p.csv")
dim <- dim(ar10p)
n <- dim[1]
p <- dim[2]-1
Y <- ar10p[,p+1]
X <- ar10p[,1:p]

# exemple train-test
train_indices <- sample(1:n,size = round(0.8*n),replace = FALSE)

X_train <- X[train_indices,] 
X_test <- X[-train_indices,]
Y_train <- Y[train_indices]
Y_test <- Y[-train_indices]
table(Y_train)

#res <- feature.selection(X_train,Y_train,50)
#res$hsic_selected_feature_index

rep <- 10
m_range <- seq(10,50,10)
m_taille <- length(m_range)
ClassifAcc_table <- matrix(0, nrow = m_taille,ncol = rep)
RED_table <- matrix(0,nrow = 1,ncol=rep)

for(m_index in seq_along(m_range)){
  m_val <- m_range[m_index]
  print(m_val)
  for(i in 1:rep){
    train_indices <- sample(1:n,size = round(0.8*n),replace = FALSE)
    X_train <- X[train_indices,] 
    X_test <- X[-train_indices,]
    Y_train <- Y[train_indices]
    Y_test <- Y[-train_indices]
  
    # on applique une méthode de selection de variable pour selectionner m_val features 
    #res <- feature.selection(X_train,Y_train,m_val)
    #features_index <- res$hsic_selected_feature_index
  
    #----------------------------------------------------
    # TABLE 3 -- MClassifAccurcay 
    #----------------------------------------------------
    # on ajuste une KLR avec X_train et Y_train 
      # cross validation 
    
      # ajustement du modèle 
    
    
    # on applique la prédiction de X_test avec le modèle entrainé 
    #prediction <- modeledKLR_fitted(X_test) 
    
    # on regarde le taux de bien classé 
    #ClassifAcc_table[,i] <- sum(Y_test == prediction)/length(Y_test)
    
    #----------------------------------------------------
    # TABLE 4 -- RED Score only for m_val = 50
    #----------------------------------------------------
    if(m_val == 50){
    RED_table[,i] <- res_score(features_index, m_val, X_train)
    }
  }
}

MeanClassifAcc_table <- rowMeans(ClassifAcc_table)
MeanRED_table <- rowMeans(RED_table)

plot(m_range,MeanClassifAcc_table,type="l")

write(MeanClassifAcc_table,"MCA_ar10p_hsic.csv",append = FALSE)
write(MeanRED_table,"MCA_ar10p_hsic.csv",append = FALSE)
