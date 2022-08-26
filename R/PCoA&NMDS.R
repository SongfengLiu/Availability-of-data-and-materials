rm(list=ls())
setwd("E:/2019珠海红树林Hg/pcoa")
library(vegan)
library(ggplot2)

#输入实验设计
design = read.table("Design.txt",header=T, sep="\t",comment.char='',check.names=FALSE)
rownames(design) <- design[,1]

#输入OTU表
otu= read.delim("5.txt", sep="\t",row.names= 1,header=T, check.names=F)
otu<-decostand(otu,"hellinger")
otu <- data.frame(t(otu))
distance <- vegdist(otu, method = 'bray')
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = TRUE)
point <- data.frame(pcoa$point)
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)
sample_site <- data.frame({pcoa$point})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')
sample_site <- merge(sample_site, design, by = 'names', all.x = TRUE)

#作图
p = ggplot(sample_site, aes(x=PCoA1, y=PCoA2, group=design$site)) +
  geom_point(aes(color = design$site), size = 3, alpha = 0.8)+
  stat_ellipse(#加上置信椭圆
               geom = 'polygon',
               level = 0.95, #置信区间95%
               alpha = 0.1, #透明度
               segments = 51, #椭圆类型
               show.legend = F)+
  labs(x=paste("PCoA 1 (", format(100 * pcoa_eig[1] / sum(pcoa_eig), digits=4), "%)", sep=""), y=paste("PCoA 2 (", format(100 * pcoa_eig[2] / sum(pcoa_eig), digits=4), "%)", sep=""), title="bray_curtis PCoA",hjust=1)+
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = '#E2EAEA'))+
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) +
  geom_hline(yintercept = 0, color = 'gray', size = 0.4)
p

#Adonis检验：, shape = design$site
adonis_result_otu <- adonis(otu~design$site, design, permutations = 999, distance = 'bray')
adonis_result_otu

#Anosim检验
anosim.result<-anosim(distance, design$site,permutations=999,distance='bray')
summary(anosim.result)

nmds<-metaMDS(otu,distance="bray")
nmds$stress
stressplot(nmds, main = "Shepard 图")
map<- scores(nmds)
map<-data.frame(map)
ggplot(data = map, aes(NMDS1, NMDS2, group=design$site)) +
  geom_point(aes(color = design$site), size = 3, alpha = 0.8)+
  stat_ellipse(#加上置信椭圆
               geom = 'polygon',
               level = 0.95, #置信区间95%
               alpha = 0.1, #透明度
               segments = 51, #椭圆类型
               show.legend = F)+
  theme_bw()+#主题
  geom_vline(xintercept = 0, color = 'black', size = 1, linetype = 3)+ #添加过原点线(y)
  geom_hline(yintercept = 0, color = 'black', size = 1, linetype = 3)+ #添加过原点线(x)
  xlab("NMDS1")+#x轴标题
  ylab("NMDS2")+#y轴标题
  theme(axis.title.y = element_text(size = 14))+ #y轴标题大小
  theme(axis.title.x = element_text(size = 14))+ #x轴标题大小
 # geom_text(aes(x=-0.8,y=0.55,label="stress = 0.07"),size = 6)+ #设置文字位置、内容和大小
  #geom_text(aes(x=-0.8,y=0.50,label="R^2 = 0.995"),size = 6)+ #添加R^2
  theme(axis.text.x=element_text(size=14,angle=0,color="Black"),#设置x和y轴字体大小和颜色
        axis.text.y=element_text(size=14,angle=0,color="Black"))+
  theme(legend.position=c(0.95,0.9))+#设置图例位置
  theme(legend.text=element_text(size=14))+ #设置图例字体大小
  theme(legend.title = element_text(face = "bold", size = 14))#设置图例标题字和大小
