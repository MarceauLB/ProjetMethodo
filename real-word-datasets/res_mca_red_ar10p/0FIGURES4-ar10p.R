rm(list=ls())

setwd("00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/res_mca_red_ar10p/")

res_delta <- read.csv("res_hsic/ar10p_hsic_mca_sigma1.csv")
res_gaussian <- read.csv("res_hsic/hsic_y_gaussian.csv",sep=",")

mean_delta <- rowMeans(res_delta)
mean_gaussian <- rowMeans(res_gaussian)

step <- seq(10,50,10)

plot(step,mean_delta,type="l",
     ylim = c(0,1),col="black",lwd=2,lty=1,
     xlab="Nombres de variables extraites",
     ylab="Taux moyen de classifications correctes")
lines(step,mean_gaussian,type="l",col="black",lwd=2,lty=2)

legend("bottomright",legend=c("Delta","Gaussien"),
       col=c("black","black"),
       lwd=2,
       lty=c(1,2),
       cex=1)



