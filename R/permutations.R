rm(list=ls())
setwd("E:/particle垂直分布/分析/permanvoa")
spp<-read.csv('group-0.2.txt',head=T,stringsAsFactors=F,sep = "\t")
otu <- read.csv("p-0.2.csv",sep=",",header=T,row.names=1)
otu=t(otu)
# PERMANOVA分析
# 整体水平比较
ad<-adonis(otu~Depth,data = spp,permutations = 999,method="bray")
write.csv(ad$aov.tab,"p-0.2-permutations.csv")
