rm(list=ls())

setwd("~/00_Ensai/projet-methodo/ProjetMethodo/simulated-dataset/r-code/data2-times-experiments/")

library(GSelection)

n <- 100
d <- 1000
set.seed(123)

X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
epsilon <- rnorm(n,0,1)
Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon


store_results <- matrix(0, ncol = 3, nrow=10, byrow = TRUE)

time_execution <- function(n,d){
  X <- as.data.frame(matrix(rnorm(n*d, mean = 0, sd = 1), nrow = n, ncol = d))
  epsilon <- rnorm(n,0,1)
  Y <- X$V1*exp(2*X$V2) + X$V3**2 + epsilon
  res_time <- system.time(feature.selection(X,Y,3))
  return(res_time[["elapsed"]])
}

col_ind <- 0
for(d in c(1000, 2000, 3000)){
  print(d)
  col_ind <- col_ind+1
  i <- 0 
  print(col_ind)
  for (n_sample in seq(100, 1000, 100)){
    i <- i+1
    list_time <- time_execution(n_sample,d)
    store_results[i,col_ind] <- list_time
    print(i)
  }}

#write.csv(store_results,file="hsic_time_samples.csv",row.names = FALSE)


rm(list=ls())
data <- read.csv("hsic_time_samples.csv")
features <- seq(100,1000,100)

plot(features, data$V3, type = "l", lty = 3, lwd = 3, 
     xlab = "Number of samples", 
     ylab = "Seconds",
     log="y", 
     col="blue")

lines(features, data$V2, lty = 2, lwd = 3, col="red")
lines(features, data$V1, lty = 1, lwd = 3,col="black")


legend("topleft", legend = c("d=1000", "d=2000", "d=3000"), 
       lty = c(1,2,3), lwd = 3,cex=1, 
       col=c("black","red","blue"))


data <- read.csv("../../../real-word-datasets/dataset_csv/ar10p.csv")
image_matrix <- matrix(as.numeric(data[1, -ncol(data)]), nrow=50, ncol=48, byrow=TRUE)  # Exclude the last column (label y)
image(t(image_matrix[nrow(image_matrix):1, ]), col=gray.colors(256))  # Flip the matrix for correct orientation
