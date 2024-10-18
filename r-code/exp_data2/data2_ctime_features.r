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

features <- seq(100,1000,100)
moyenne_lignes <- rowMeans(store_results)

plot(features, moyenne_lignes, type = "l", 
     xlab = "Numbers of features", 
     ylab = "Seconds",
     main = "Computation time of different models depending on the number of selected variables", 
     ylim = c(1, 10^2 +10),
     log="y",
     yaxt = "n")  # Suppresses default y-axis labels

# Customize y-axis with no labels, only ticks
axis(2, at = c(0,1,10, 100), labels = FALSE)  # Suppresses default labels but adds ticks

# Add rotated y-axis labels using text() with 45 degrees rotation
text(x = rep(par("usr")[1], 4), y = c(0,1,10, 100), 
     labels = expression(0, 1, 10^1, 10^2), 
     srt = 0,  # Rotate labels by 0 degrees
     pos = 2,   # Position to the left of the axis
     xpd = TRUE,  # Allow text outside plot region
     adj = 1)  # Adjust alignment for rotated labels






