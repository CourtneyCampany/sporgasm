source("master_scripts/plot_objects.R")

#sla

sla <- read.csv("calculated_data/fern_sla.csv")

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

sla_lab <- expression(SLA~~(cm^2~g^-1))

#reorder from ground to canopy 
sla$niche2<-factor(sla$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")


jpeg(filename = "output/sla.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)

boxplot(sla_cm2g ~ niche2, data=sla,xaxt='n',ylim=c(0, 225),
        ylab=sla_lab, outline=FALSE, border=trtcols, varwidth=TRUE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(sla_cm2g ~ niche2, data = sla,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=157, cldstomata)

dev.off()