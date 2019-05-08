#plot kmax scaled to leaf area from vulnerability curves

kleaf <- read.csv("calculated_data/kleaf.csv")

#reorder from ground to canopy 
kleaf$niche2<-factor(kleaf$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


#plotbits
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

 # windows()
par(mgp=c(2.5,1,0),mar=c(4,4,1,1), cex.lab=1)

#kmax leaf
boxplot(kmax_leaf ~ niche2, data=kleaf, xaxt='n',ylim=c(0, .0125),
        ylab="Kmax (leaf area corrected)",
        varwidth=TRUE,border=trtcols)
axis(1, labels=boxlabs, at=1:3)
stripchart(kmax_leaf ~ niche2, data = kleaf,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

