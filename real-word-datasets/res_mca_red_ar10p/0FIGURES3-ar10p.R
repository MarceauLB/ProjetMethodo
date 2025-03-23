rm(list=ls())
setwd("~/00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/res_mca_red_ar10p/")

abs <- seq(10,50,10)
hsic_sigma05 <- read.csv("res_hsic/hsic_delta_sigma05.csv",header = TRUE)
hsic_sigma15 <- read.csv("res_hsic/hsic_delta_sigma15.csv",header = TRUE)
hsic_sigma1 <- read.csv("res_hsic/ar10p_hsic_mca_sigma1.csv")

s05 <- rowMeans(hsic_sigma05)
s1 <- rowMeans(hsic_sigma1)
s15 <- rowMeans(hsic_sigma15)

plot(abs,s1,type="l",
     ylim=c(0.5,1),
     col="black",lwd=2,
     xlab = "Number of extracted features",
     ylab="Mean Classification Accuracy")
lines(abs,s05,col="blue",lwd=2,lty=2) # +seq(1:5)/130
lines(abs,s15,col="red",lwd=2,lty=2) # +seq(1:5)/150

legend("topleft",
       legend = c(expression(sigma[x] == 0.5~" "), 
                  expression(sigma[x] == 1~" "), 
                  expression(sigma[x] == 1.5~" ")),
       col = c("blue", "black", "red"),
       lwd = 2,lty = c(2, 1, 2))