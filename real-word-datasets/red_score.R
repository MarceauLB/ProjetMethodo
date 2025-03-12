# RED function 
rm(list=ls())

res_score <- function(features_index, m_val, X){
  features_values <- X[,features_index]  
  correlation_matrix <- cor(X)
  red <- 0 
  for(l in 1:(m_val-1)){
    for(k in (l+1):m_val){
      red <- red + abs(correlation_matrix[k,l])
    }
  }
  red <- 2*(red/(m_val*(m_val-1)))
  return(red)
}