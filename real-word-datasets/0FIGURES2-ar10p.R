setwd("../../../real-word-datasets/res_mca_red_ar10p/")

mrmr <- read.csv("ar10p_mrmr_mca.csv", sep="", header=FALSE)
lasso <- read.csv("ar10p_lasso_mca.csv", sep="", header=FALSE)
enet <- read.csv("ar10p_enet_mca.csv", sep="", header=FALSE)

red_mrmr <- read.csv("ar10p_mrmr_red50.csv", header=FALSE,sep="")
red_lasso <- read.csv("ar10p_enet_red50.csv", header=FALSE, sep="")
red_enet <- read.csv("ar10p_lasso_red50.csv", header=FALSE, sep="")


cmean_mrmr <- colMeans(mrmr)
cmean_lasso <- colMeans(lasso)
cmean_enet <- colMeans(enet)

sqrt(var(red_mrmr$V5))
sqrt(var(red_lasso$V5))
sqrt(var(red_enet$V5))


#-----------------------------------------
red_mrmr_vec <- as.vector(as.matrix(red_mrmr))
red_lasso_vec <- as.vector(as.matrix(red_lasso))
red_enet_vec <- as.vector(as.matrix(red_enet))

mean(red_mrmr_vec)
sqrt(var(red_mrmr_vec))

mean(red_lasso_vec)
sqrt(var(red_lasso_vec))

mean(red_enet_vec)
sqrt(var(red_enet_vec))

plot(features_index,cmean_mrmr,type="l",col="darkgreen",ylim=c(0,1),
     main="Mean Classification Accuracy",
     xlab ="Top Features Selections")
lines(features_index,cmean_enet,type="l",col="salmon",ylim=c(0,1))
lines(features_index,cmean_lasso,type="l",col="purple",ylim=c(0,1))


# Add a legend
legend("bottomright", legend = c("HSIC", "Spam", "mRMR", "Lasso", "CKTA", "QPFS", "ENet"), 
       col = c("red", "blue", "darkgreen", "purple", "orange", "brown", "salmon"), 
       lwd = 1, cex = 0.6)



#col = "blue", lwd = 1)   # Spam
#col = "darkgreen", lwd = 1) # mRMR
#col = "purple", lwd = 1) # Lasso
#col = "orange", lwd = 1) # CKTA
#"brown", lwd = 1) # QPFS
#"salmon", lwd = 1) # ENet
