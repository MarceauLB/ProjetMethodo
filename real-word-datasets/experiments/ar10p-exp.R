rm(list=ls())
setwd("~/00_Ensai/projet-methodo/ProjetMethodo/real-word-datasets/experiments/")

ar10p <- read.csv("../dataset_csv/ar10p.csv")


Y <- ar10p[,2401]
X <- ar10p[,1:2400]
table(Y)

