rm(list=ls())
setwd("E:/2019�麣������Hg/pcoa")
library(vegan)
library(ggplot2)

#����ʵ�����
design = read.table("Design.txt",header=T, sep="\t",comment.char='',check.names=FALSE)
rownames(design) <- design[,1]

#����OTU��
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

#��ͼ
p = ggplot(sample_site, aes(x=PCoA1, y=PCoA2, group=design$site)) +
  geom_point(aes(color = design$site), size = 3, alpha = 0.8)+
  stat_ellipse(#����������Բ
               geom = 'polygon',
               level = 0.95, #��������95%
               alpha = 0.1, #͸����
               segments = 51, #��Բ����
               show.legend = F)+
  labs(x=paste("PCoA 1 (", format(100 * pcoa_eig[1] / sum(pcoa_eig), digits=4), "%)", sep=""), y=paste("PCoA 2 (", format(100 * pcoa_eig[2] / sum(pcoa_eig), digits=4), "%)", sep=""), title="bray_curtis PCoA",hjust=1)+
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = '#E2EAEA'))+
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) +
  geom_hline(yintercept = 0, color = 'gray', size = 0.4)
p

#Adonis���飺, shape = design$site
adonis_result_otu <- adonis(otu~design$site, design, permutations = 999, distance = 'bray')
adonis_result_otu

#Anosim����
anosim.result<-anosim(distance, design$site,permutations=999,distance='bray')
summary(anosim.result)

nmds<-metaMDS(otu,distance="bray")
nmds$stress
stressplot(nmds, main = "Shepard ͼ")
map<- scores(nmds)
map<-data.frame(map)
ggplot(data = map, aes(NMDS1, NMDS2, group=design$site)) +
  geom_point(aes(color = design$site), size = 3, alpha = 0.8)+
  stat_ellipse(#����������Բ
               geom = 'polygon',
               level = 0.95, #��������95%
               alpha = 0.1, #͸����
               segments = 51, #��Բ����
               show.legend = F)+
  theme_bw()+#����
  geom_vline(xintercept = 0, color = 'black', size = 1, linetype = 3)+ #���ӹ�ԭ����(y)
  geom_hline(yintercept = 0, color = 'black', size = 1, linetype = 3)+ #���ӹ�ԭ����(x)
  xlab("NMDS1")+#x�����
  ylab("NMDS2")+#y�����
  theme(axis.title.y = element_text(size = 14))+ #y������С
  theme(axis.title.x = element_text(size = 14))+ #x������С
 # geom_text(aes(x=-0.8,y=0.55,label="stress = 0.07"),size = 6)+ #��������λ�á����ݺʹ�С
  #geom_text(aes(x=-0.8,y=0.50,label="R^2 = 0.995"),size = 6)+ #����R^2
  theme(axis.text.x=element_text(size=14,angle=0,color="Black"),#����x��y�������С����ɫ
        axis.text.y=element_text(size=14,angle=0,color="Black"))+
  theme(legend.position=c(0.95,0.9))+#����ͼ��λ��
  theme(legend.text=element_text(size=14))+ #����ͼ�������С
  theme(legend.title = element_text(face = "bold", size = 14))#����ͼ�������ֺʹ�С