setwd("~/00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/dataset/")

library(GSelection)
library(R.matlab)


rm(list=ls())
data <- readMat("warpAR10P.mat")
str(data)
ar10p <- as.data.frame(data$X)
colnames(ar10p) <- paste0("X", 1:2400)
ar10p$y <- as.vector(data$Y) 

write.csv(ar10p,"../to_csv/ar10p.csv",row.names = FALSE)


rm(list=ls())
data <- readMat("warpPIE10P.mat")
str(data)
pie10p <- as.data.frame(data$X)
colnames(pie10p) <- paste0("X", 1:2420)
pie10p$y <- as.vector(data$Y) 

write.csv(pie10p,"../to_csv/pie10p.csv",row.names = FALSE)


rm(list=ls())
data <- readMat("pixraw10P.mat")
str(data)
pix10p <- as.data.frame(data$X)
colnames(pix10p) <- paste0("X", 1:10000)
pix10p$y <- as.vector(data$Y) 

write.csv(pix10p,"../to_csv/pix10p.csv",row.names = FALSE)


rm(list=ls())
data <- readMat("orlraws10P.mat")
str(data)
orl10p <- as.data.frame(data$X)
colnames(orl10p) <- paste0("X", 1:10304)
orl10p$y <- as.vector(data$Y) 

write.csv(orl10p,"../to_csv/orl10p.csv",row.names = FALSE)



rm(list=ls())
data <- readMat("TOX-171.mat")
str(data)
tox <- as.data.frame(data$X)
colnames(tox) <- paste0("X", 1:5748)
tox$y <- as.vector(data$Y) 

write.csv(tox,"../to_csv/tox.csv",row.names = FALSE)


rm(list=ls())
data <- readMat("CLL-SUB-111.mat")
str(data)
cll_sub <- as.data.frame(data$X)
colnames(cll_sub) <- paste0("X", 1:11340)
cll_sub$y <- as.vector(data$Y) 

write.csv(cll_sub,"../to_csv/cll_sub.csv",row.names = FALSE)
