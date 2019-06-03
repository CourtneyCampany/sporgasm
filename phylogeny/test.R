#indepedent constatst examples

obj<-read.csv("constrasts/Centrarchidae.csv",row.names=1)



fit.ols<-lm(gape.width~buccal.length,data=obj)
fit.ols
summary(fit.ols)


plot(obj[,c("buccal.length","gape.width")],
     xlab="relative buccal length",
     ylab="relative gape width",pch=21,bg="grey",
     cex=1.4)
abline(fit.ols,lwd=2,lty="dashed",col="red")


library(ape)
cent.tree<-read.tree("constrasts/Centrarchidae.tre")
buccal.length<-setNames(obj[,"buccal.length"],
                        rownames(obj))
gape.width<-setNames(obj[,"gape.width"],rownames(obj))
pic.bl<-pic(buccal.length,cent.tree)
pic.gw<-pic(gape.width,cent.tree)
fit.pic<-lm(pic.gw~pic.bl+0)
fit.pic
summary(fit.pic)

plot(pic.bl,pic.gw,xlab="PICs for buccal length",
     ylab="PICs for gape width",bg="grey",
     cex=1.4,pch=21)
abline(fit.pic,lwd=2,lty="dashed",col="red")



library(phytools)
