rm(list = ls())
setwd("E:/particle垂直分布/分析/new")
otu<- read.csv("n-j.csv",sep=",", row.names = 1)
otu=t(otu)
b<-beta.multi.abund(otu, index.family="bray")
b
