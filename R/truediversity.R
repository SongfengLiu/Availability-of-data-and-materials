rm(list = ls())
setwd("E:/particle??ֱ?ֲ?/????/new")
otu<- read.csv("n-otu.csv",sep=",", row.names = 1)
size<- read.csv("td-n-depth.csv",sep=",")
otu=t(otu)
otu1=melt(otu)
#write.csv(otu1, 'otu1.csv', quote = FALSE)
qs <- c(0:10)
div <- by(otu1, size, function(x) data.frame(t(sapply(c(0:10),function(y) trudi(mama(x), y)))))
write.csv(div$A, 'n-d-A.csv', quote = FALSE)
write.csv(div$B, 'n-d-B.csv', quote = FALSE)
write.csv(div$C, 'n-d-C.csv', quote = FALSE)
write.csv(div$D, 'n-d-D.csv', quote = FALSE)
write.csv(div$E, 'n-d-E.csv', quote = FALSE)
write.csv(div$F, 'n-d-F.csv', quote = FALSE)
write.csv(div$G, 'n-d-G.csv', quote = FALSE)
write.csv(div$H, 'n-d-H.csv', quote = FALSE)
write.csv(div$I, 'n-d-I.csv', quote = FALSE)
write.csv(div$J, 'n-d-J.csv', quote = FALSE)