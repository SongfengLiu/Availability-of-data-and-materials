rm(list = ls())
setwd("E:/particle??ֱ?ֲ?/????/new")
otu<- read.csv("n-j.csv",sep=",", row.names = 1)
otu=t(otu)
b<-beta.multi.abund(otu, index.family="bray")
b
