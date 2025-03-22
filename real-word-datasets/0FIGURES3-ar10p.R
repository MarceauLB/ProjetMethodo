# Figure 3 

rm(list=ls())
setwd("~/00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/res_mca_red_ar10p/")

abs <- seq(10,50,10)
hsic_sigma05 <- read.csv("res_hsic/hsic_sigma05_value.csv",header = FALSE)
hsic_sigma15 <- read.csv("res_hsic/hsic_sigma15_value.csv",header = FALSE)
hsic_sigma1 <- read.csv("res_hsic/ar10p_hsic_mca_sigma1.csv",header=FALSE)

hsic_sigma1==hsic_sigma15
hsic_sigma05==hsic_sigma15
s05 <- rowMeans(hsic_sigma05)
s1 <- rowMeans(hsic_sigma1)
s15 <- rowMeans(hsic_sigma15)

plot(abs,s05,type="l",
     ylim=c(0.60,0.83),col="red")
lines(abs,s15)
lines(abs,s1)
