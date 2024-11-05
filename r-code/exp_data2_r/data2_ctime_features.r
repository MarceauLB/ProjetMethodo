rm(list=ls())

library(GSelection)

n <- 100
d <- 1000
set.seed(123)


X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
epsilon <- rnorm(n,0,1)
Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon

store_results <- matrix(0, ncol = 30, nrow=10, byrow = TRUE)

time_execution <- function(d){
  X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
  epsilon <- rnorm(n,0,1)
  Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon
  times_30 <- numeric(30)
  for(i in 1:30){
  res_time <- system.time(feature.selection(X,Y,d=3))
  times_30[i] <- res_time[["elapsed"]]
  }
  return(times_30)
}

for (nb_feature_generated in seq(100, 1000, 100)){
  list_time <- time_execution(nb_feature_generated)
  store_results[nb_feature_generated/100,] <- list_time
}

write.csv(store_results,file="hsic_time_features.csv",row.names = FALSE)

rm(list=ls())

store_results_data <- read.csv("hsic_time_features.csv")

features <- seq(100,1000,100)
moyenne_lignes <- rowMeans(store_results_data)

plot(features, moyenne_lignes, type = "l", 
     xlab = "Numbers of features", 
     ylab = "Seconds",
     main = "Computation time of different models depending on the number of selected variables", 
     ylim = c(1, 10^2 +10),
     log="y",
     col="black", lty=1)


legend("topleft", 
       legend = c("HSIC Lasso","SpAM", "QPFS"), 
       col = c("black", "green", "red"),
       lty = c(1,2,2), 
       lwd = 1,cex=0.8)



