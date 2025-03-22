# figure data1 
rm(list=ls())
setwd("~/00_Ensai/projet-methodo/ProjetMethodo/simulated-dataset/r-code/rate_features_data1/")

res_lasso <- read.csv("selected_lasso_data1.csv")
res_spam <- read.csv("selected_spam_data1.csv")
res_mrmr <- read.csv("selected_mrmr_data1.csv")
res_hsic <- read.csv("selected_hsic_data1_python.csv")
res_ckta <- read.csv("selected_ckta_data1.csv")
res_qpfs <- read.csv("selected_qpfs_data1.csv")
res_enet <- read.csv("selected_ENet_data1.csv")
res_nocco <- read.csv("selected_nocco_data1_python.csv")

mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_spam[1] <- mean(mean_row_spam[c(1:3)])
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_ckta <- rowMeans(res_ckta)
mean_row_qpfs <- rowMeans(res_qpfs)
mean_row_enet <- rowMeans(res_enet)
mean_row_hsic <- as.vector(colMeans(res_hsic))
mean_row_nocco <- as.vector(colMeans(res_nocco))

sample_seq <- seq(25, 250, by = 25)

plot(sample_seq, mean_row_hsic, type = "l", 
     xlab = "Sample Size", 
     ylab = "Rate of Correctly Selected Features",
     col = "black", lwd = 2,
     ylim=range(c(0,1)))

lines(sample_seq, mean_row_nocco, col = "blue", lwd = 2) # Nocco Lasso 
lines(sample_seq, mean_row_spam / 100, col = "green2", lwd = 2)   # Spam
lines(sample_seq, mean_row_mrmr / 100, col = "cyan2", lwd = 2) # mRMR
lines(sample_seq, mean_row_ckta / 100, col = "yellow3", lwd = 2) # CKTA
lines(sample_seq, mean_row_qpfs / 100, col = "red2", lwd = 2) # QPFS
lines(sample_seq, mean_row_enet / 100, col = "orange2", lwd = 2) # ENet
lines(sample_seq, mean_row_lasso / 100, col = "purple2", lwd = 2) # Lasso
abline(h=0.7)
# Add a legend
legend("bottomright", legend = c("HSIC", "SPAM", "mRMR", "Lasso", "cKTA", "QPFS", "ENet","NOCCO"), 
       col = c("black", "green2", "cyan2", "purple2", "yellow3", "red2", "orange2","blue"), 
       lwd = 2, cex = 1)

# figure data2 
rm(list=ls())
setwd("~/00_Ensai/projet-methodo/ProjetMethodo/simulated-dataset/r-code/rate_features_data2/")

res_lasso <- read.csv("selected_lasso_data2.csv")
res_spam <- read.csv("selected_spam_data2.csv")
res_mrmr <- read.csv("selected_mrmr_data2.csv")
res_hsic <- read.csv("selected_hsic_data2.csv")
res_hsic_py <- read.csv("selected_hsic_data1_python.csv")
res_ckta <- read.csv("selected_ckta_data2.csv")
res_qpfs <- read.csv("selected_qpfs_data2.csv")
res_enet <- read.csv("selected_ENet_data2.csv")
res_nocco <- read.csv("selected_nocco_data2_python.csv")


mean_row_lasso <- rowMeans(res_lasso)
mean_row_spam <- rowMeans(res_spam)
mean_row_spam[1] <- sum(mean_row_spam[2:3])/3
mean_row_mrmr <- rowMeans(res_mrmr)
mean_row_hsic <- rowMeans(res_hsic)
mean_row_ckta <- rowMeans(res_ckta)
mean_row_qpfs <- rowMeans(res_qpfs)
mean_row_enet <- rowMeans(res_enet)
mean_row_nocco <- rowMeans(res_nocco)

sample_seq <- seq(25, 250, by = 25)

plot(sample_seq, mean_row_hsic / 100, type = "l", 
     xlab = "Sample Size", 
     ylab = "Rate of Correctly Selected Features",
     col = "black", lwd = 2,
     ylim=range(c(0,1)))

lines(sample_seq, mean_row_spam / 100, col = "green2", lwd = 2)   # Spam
lines(sample_seq, mean_row_mrmr / 100, col = "cyan2", lwd = 2) # mRMR
lines(sample_seq, mean_row_lasso / 100, col = "purple2", lwd = 2) # Lasso
lines(sample_seq, mean_row_ckta / 100, col = "yellow3", lwd = 2) # CKTA
lines(sample_seq, mean_row_qpfs / 100, col = "red2", lwd = 2) # QPFS
lines(sample_seq, mean_row_enet / 100, col = "orange2", lwd = 2) # ENet
lines(sample_seq, mean_row_nocco, col = "blue", lwd = 2) # Nocco Lasso 

# Add a legend
legend("bottomright", legend = c("HSIC", "SPAM", "mRMR", "Lasso", "cKTA", "QPFS", "ENet","NOCCO"), 
       col = c("black", "green2", "cyan2", "purple2", "yellow3", "red2", "orange2","blue"), 
       lwd = 2, cex = 0.8)

