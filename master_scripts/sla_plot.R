#sla

sla <- read.csv("calculated_data/fern_sla.csv")


#habitat colors
gradient <- colorRampPalette(c("forestgreen","orange"))
palette(gradient(3))
trtcols <- palette(gradient(3))
boxlabs <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")
sla_lab <- expression(SLA~~(g~m^-2))

#reorder from ground to canopy 
sla$niche2<-factor(sla$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")


jpeg(filename = "output/sla.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))

boxplot(sla_cm2g ~ niche2, data=sla,xaxt='n',ylim=c(0, 225),
        ylab=sla_lab, outline=FALSE, col=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1,cex.axis=1.25)

dev.off()