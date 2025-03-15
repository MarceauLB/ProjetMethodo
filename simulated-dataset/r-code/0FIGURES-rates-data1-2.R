# figure data1 
rm(list=ls())
setwd("../r-code/rate_features_data1/")

res_lasso <- read.csv("selected_lasso_data1.csv")
res_spam <- read.csv("selected_spam_data1.csv")
res_mrmr <- read.csv("selected_mrmr_data1.csv")
res_hsic <- read.csv("selected_hsic_data1.csv")
res_ckta <- read.csv("selected_ckta_data1.csv")
res_qpfs <- read.csv("selected_qpfs_data1.csv")
res_enet <- read.csv("selected_ENet_data1.csv")

mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_spam[1] <- mean(mean_row_spam[c(1:3)])
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)
mean_row_ckta <- rowMeans(res_ckta)
mean_row_qpfs <- rowMeans(res_qpfs)
mean_row_enet <- rowMeans(res_enet)

echantillon_points <- seq(25, 250, by = 25)

plot(echantillon_points, mean_row_hsic / 100, type = "l", 
     xlab = "Sample Size", 
     ylab = "Rate of Correctly Selected Features",
     main = "Rate of Correctly Selected Features by Sample Size",
     col = "black", lwd = 2,
     ylim=range(c(0,1)))

lines(echantillon_points, mean_row_spam / 100, col = "green", lwd = 2)   # Spam
lines(echantillon_points, mean_row_mrmr / 100, col = "cyan", lwd = 2) # mRMR
lines(echantillon_points, mean_row_lasso / 100, col = "purple", lwd = 2) # Lasso
lines(echantillon_points, mean_row_ckta / 100, col = "yellow", lwd = 2) # CKTA
lines(echantillon_points, mean_row_qpfs / 100, col = "red", lwd = 2) # QPFS
lines(echantillon_points, mean_row_enet / 100, col = "salmon", lwd = 2) # ENet
#lines(echantillon_points, mean_row_nocco / 100, col = "blue", lwd = 2) # Nocco Lasso 

# Add a legend
legend("bottomright", legend = c("HSIC", "Spam", "mRMR", "Lasso", "CKTA", "QPFS", "ENet"), 
       col = c("black", "green", "cyan", "purple", "yellow", "red", "salmon"), 
       lwd = 2, cex = 0.6)

# figure data2 
rm(list=ls())
setwd("../rate_features_data2/")

res_lasso <- read.csv("selected_lasso_data2.csv")
res_spam <- read.csv("selected_spam_data2.csv")
res_mrmr <- read.csv("selected_mrmr_data2.csv")
res_hsic <- read.csv("selected_hsic_data2.csv")
res_ckta <- read.csv("selected_ckta_data2.csv")
res_qpfs <- read.csv("selected_qpfs_data2.csv")
res_enet <- read.csv("selected_ENet_data2.csv")


mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_spam[1] <- 0 
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)
mean_row_ckta <- rowMeans(res_ckta)
mean_row_qpfs <- rowMeans(res_qpfs)
mean_row_enet <- rowMeans(res_enet)

echantillon_points <- seq(25, 250, by = 25)

plot(echantillon_points, mean_row_hsic / 100, type = "l", 
     xlab = "Sample Size", 
     ylab = "Rate of Correctly Selected Features",
     main = "Rate of Correctly Selected Features by Sample Size",
     col = "black", lwd = 2,
     ylim=range(c(0,1)))

lines(echantillon_points, mean_row_spam / 100, col = "green", lwd = 2)   # Spam
lines(echantillon_points, mean_row_mrmr / 100, col = "cyan", lwd = 2) # mRMR
lines(echantillon_points, mean_row_lasso / 100, col = "purple", lwd = 2) # Lasso
lines(echantillon_points, mean_row_ckta / 100, col = "yellow", lwd = 2) # CKTA
lines(echantillon_points, mean_row_qpfs / 100, col = "red", lwd = 2) # QPFS
lines(echantillon_points, mean_row_enet / 100, col = "salmon", lwd = 2) # ENet
#lines(echantillon_points, mean_row_nocco / 100, col = "blue", lwd = 2) # Nocco Lasso 

# Add a legend
legend("bottomright", legend = c("HSIC", "Spam", "mRMR", "Lasso", "CKTA", "QPFS", "ENet"), 
       col = c("black", "green", "cyan", "purple", "yellow", "red", "salmon"), 
       lwd = 2, cex = 0.6)

