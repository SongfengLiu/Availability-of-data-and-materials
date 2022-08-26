rm(list=ls())
setwd("E:/particle垂直分布/傅里叶离子回旋共振质谱/analysis")
data <- read.csv("venn.csv", sep = ",", header=T,stringsAsFactors=F)
ggplot(data, aes(x=O.C, y=H.C,color=X)) +geom_point(size=4)+scale_y_continuous(breaks = seq(0,2.5, 0.5), limits = c(0,2.5))+scale_color_manual(values = c("#FFD700","#00FF7F","#00EE76","#00CD66","#008B45","#ff4500","#003399"))+
  scale_x_continuous(breaks = seq(0,1, 0.25), limits = c(0,1))+
  theme_classic() +  
  theme(panel.background = element_rect(fill = "white",colour = "black",size = 0.25),axis.line =element_line(colour = "black",   size = 0.25),axis.title =element_text(size = 13, face = "plain", color = "black"),axis.text = element_text(size =12, face =    "plain", color = "black"))

setwd("E:/particle垂直分布/傅里叶离子回旋共振质谱/analysis/point")
data <- read.csv("FTMS_sediment.csv", sep = ",", header=T,stringsAsFactors=F)
ggplot(data, aes(x=O.C, y=H.C,color=ElementComposition,size=RelativeAbundance)) +geom_point()+scale_y_continuous(breaks = seq(0,2.5, 0.5), limits = c(0,2.5))+scale_color_manual(values = c("#996633","#fac090","#cfcfcf","#8a9aad"))+
  scale_x_continuous(breaks = seq(0,1, 0.25), limits = c(0,1))+
  theme_classic() +  
  theme(panel.background = element_rect(fill = "white",colour = "black",size = 0.25),axis.line =element_line(colour = "black",   size = 0.25),axis.title =element_text(size = 13, face = "plain", color = "black"),axis.text = element_text(size =12, face =    "plain", color = "black"))

