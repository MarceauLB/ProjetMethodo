rm(list=ls())

gaussian <- read.csv("res_hsic/hsic_res_gaussian_fromR.csv",header=TRUE)
gaussian_py <- read.csv("res_hsic/hsic_y_gaussian.csv",header=FALSE)
delta <- read.csv("res_hsic/ar10p_hsic_mca_sigma1.csv",header = FALSE)

mean_g <- rowMeans(as.matrix(gaussian))
mean_g_py <- rowMeans(as.matrix(gaussian_py))
mean_d <- rowMeans(as.matrix(delta))

step <- seq(10,50,10)
plot(step,mean_g,type="l",ylim = c(0,1),col="red")
lines(step,mean_d,type="l",col="blue")
lines(step,mean_g_py,type="l",col="green3")




legend("bottomleft",legend=c("hsic_delta_py","hsic_R","hsic_gaussian_py"),
       fill=c("blue","red","green3"))

