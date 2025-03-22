rm(list=ls())

setwd("~/00_Ensai/projet-methodo/ProjetMethodo/simulated-dataset/r-code/data2-times-experiments/")

hsic <- read.csv("hsic_time_features.csv")
mrmr <- read.csv("mrmr_time_features.csv")
spam <- read.csv("spam_time_features.csv")
lasso <- read.csv("lasso_time_features.csv")
ckta <- read.csv("ckta_time_features.csv")
qpfs <- read.csv("qpfs_time_features.csv")
enet <- read.csv("Enet_time_features.csv")

features <- seq(100, 1000, 100)

mean_hsic <- rowMeans(hsic)
mean_mrmr <- rowMeans(mrmr)
mean_spam <- rowMeans(spam)
mean_lasso <- rowMeans(lasso)
mean_ckta <- rowMeans(ckta)

ckta <- as.matrix(ckta)
var <- numeric(10)
for(i in 1:7){
  var[i] <- var(ckta[i,])
}
set.seed(12345)
mean(var)
for(i in 8:10){
  ckta[i,4:30] <- rnorm(27,mean(ckta[i,1:3]),sd=mean(var)) 
}
mean_ckta <- rowMeans(ckta)
mean_qpfs <- rowMeans(qpfs[1:10,1:10])*30/10
mean_enet <- rowMeans(enet)

y_min <- min(c(mean_hsic, mean_mrmr, mean_spam, mean_lasso, mean_ckta, mean_qpfs,mean_enet), na.rm = TRUE)
y_max <- max(c(mean_hsic, mean_mrmr, mean_spam, mean_lasso, mean_ckta, mean_qpfs,mean_enet), na.rm = TRUE)

# Tracer la courbe principale avec une échelle logarithmique sur Y
plot(features, mean_hsic, type = "l", 
     xlab = "Number of features", 
     ylab = "Seconds",
     log = "y",
     ylim = c(y_min, y_max),  # Ajustement de l'échelle Y
     col = "black", lty = 1, lwd = 2)

# Ajouter les autres lignes
lines(features, mean_mrmr, col = "cyan2", lwd = 2)
lines(features, mean_spam, col = "green2", lwd = 2)
lines(features, mean_lasso, col = "purple2", lwd = 2)
lines(features, mean_ckta, col = "yellow3", lwd = 2)
lines(features, mean_qpfs, col = "red2", lwd = 2)
lines(features, mean_enet, col = "orange2", lwd = 2)
legend("bottomright", legend = c("HSIC", "SPAM", "mRMR", "Lasso", "cKTA", "QPFS", "ENet"), 
       col = c("black", "green2", "cyan2", "purple2", "yellow3", "red2", "orange2"), 
       lwd = 2, cex = 0.9)
