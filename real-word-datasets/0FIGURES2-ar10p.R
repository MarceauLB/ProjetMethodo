setwd("~/00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/res_mca_red_ar10p/")
rm(list=ls())

mrmr <- read.csv("ar10p_mrmr_mca.csv", sep="", header=FALSE)
lasso <- read.csv("ar10p_lasso_mca.csv", sep="", header=FALSE)
enet <- read.csv("ar10p_enet_mca.csv", sep="", header=FALSE)
ckta <- read.csv("ar10p_ckta_mca.csv", sep="", header=FALSE)
ckta <- ckta[1:13,]
qpfs <- read.csv("ar10p_qpfs_mca.csv", sep="", header=FALSE)
hsic <- read.csv("ar10p_hsic_mca.csv", sep=",", header=FALSE)

red_mrmr <- read.csv("ar10p_mrmr_red50.csv", header=FALSE,sep="")
red_lasso <- read.csv("ar10p_enet_red50.csv", header=FALSE, sep="")
red_enet <- read.csv("ar10p_lasso_red50.csv", header=FALSE, sep="")
red_ckta <- read.csv("ar10p_ckta_red50.csv")
red_qpfs <- read.csv("ar10p_qpfs_red50.csv", header=FALSE, sep="")
red_hsic <- read.csv("ar10p_hsic_red50.csv", header=FALSE, sep=",")


cmean_mrmr <- colMeans(mrmr)
cmean_lasso <- colMeans(lasso)
cmean_enet <- colMeans(enet)
cmean_ckta <- colMeans(ckta)
cmean_qpfs <- colMeans(qpfs)
cmean_hsic <- rowMeans(hsic)

# - variance associé à la colonne 5
sqrt(var(mrmr$V5))
sqrt(var(lasso$V5))
sqrt(var(enet$V5))
sqrt(var(ckta$V5))
sqrt(var(qpfs$V5))
hsic <- as.matrix(hsic)
sqrt(var(hsic[5,]))


# red score mean and S.D
red_mrmr_vec <- as.vector(as.matrix(red_mrmr))
red_lasso_vec <- as.vector(as.matrix(red_lasso))
red_enet_vec <- as.vector(as.matrix(red_enet))
red_ckta_vec <- as.vector(as.matrix(red_ckta))
red_qpfs_vec <- as.vector(as.matrix(red_qpfs))
red_hsic_vec <- as.vector(as.matrix(red_hsic))

mean(red_mrmr_vec)
sqrt(var(red_mrmr_vec))

mean(red_lasso_vec)
sqrt(var(red_lasso_vec))

mean(red_enet_vec)
sqrt(var(red_enet_vec))

mean(red_ckta_vec)
sqrt(var(red_ckta_vec))

mean(red_qpfs_vec)
sqrt(var(red_qpfs_vec))

mean(red_hsic_vec)
sqrt(var(red_hsic_vec))


features_index <- seq(10,50,10)
plot(features_index,cmean_hsic,type="l",col="black",ylim=c(0,1),
     xlab ="Number of extracted features",
     ylab="Mean classification accuracy",
     lwd=2)
lines(features_index,cmean_enet,type="l",col="orange2",lwd=2)
lines(features_index,cmean_lasso,type="l",col="purple2",lwd=2)
lines(features_index,cmean_ckta,type="l",col="yellow3",lwd=2)
lines(features_index,cmean_qpfs,type="l",col="red2",lwd=2)
lines(features_index,cmean_mrmr,type="l",col="cyan2",lwd=2)
lines(features_index,cmean_hsic,type="l",col="black",lwd=2)
legend("bottomright", 
    legend = c("HSIC",  "mRMR", "Lasso",     "cKTA", "QPFS", "ENet"), 
       col = c("black","cyan2", "purple2", "yellow3", "red2", "orange2"), 
       lwd = 2, cex = 0.8)


hsic_gaussian <- read.csv("hsic_res_gaussian.csv",header = TRUE)
hsic_gaussian <- as.matrix(hsic_gaussian)
gs <- rowMeans(hsic_gaussian)
