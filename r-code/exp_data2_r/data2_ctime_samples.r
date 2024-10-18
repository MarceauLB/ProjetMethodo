rm(list=ls())

library(GSelection)

n <- 100
d <- 1000
set.seed(123)

X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
epsilon <- rnorm(n,0,1)
Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon

store_results <- matrix(0, ncol = 30, nrow=30, byrow = TRUE)

time_execution <- function(n,d){
  X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
  epsilon <- rnorm(n,0,1)
  Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon
  times_30 <- numeric(30)
  for(i in 1:30){
    res_time <- system.time(feature.selection(X,Y,3))
    times_30[i] <- res_time[["elapsed"]]
  }
  return(times_30)
}

i <- 0
for(d in c(1000, 2000, 3000)){
for (n_sample in seq(100, 1000, 100)){
  i <- i+1
  print(i)
  list_time <- time_execution(n_sample,d)
  store_results[i,] <- list_time
  print(i)
}}


write.csv(store_results,file="hsic_time_samples.csv",row.names = FALSE)




features <- seq(100,1000,100)
moyenne_lignes <- rowMeans(store_results)

plot(features, moyenne_lignes[1:10], type = "l", lty = 1, lwd = 1, 
     xlab = "Numbers of samples", 
     ylab = "Seconds",
     main = "Computation time depending on sample size for differents features size for HSIC Lasso", 
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


lines(features, moyenneligne[11:20], lty = 2, lwd = 1)
lines(features, moyenneligne[21:30], lty = 3, lwd = 1)


legend("topleft", legend = c("d=1000", "d=2000", "d=3000"), 
       lty = c(1, 2,3), lwd = 1)



