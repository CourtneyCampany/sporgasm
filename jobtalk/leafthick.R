source("master_scripts/plot_objects.R")

#sla

sla <- read.csv("calculated_data/fern_sla.csv")
  sla$lma <- (1/sla$sla_cm2g)/.0001 #gm2
  #reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

  sla_nohemi <- droplevels(sla[!sla$niche2 == "hemi-epiphyte",])

#plot bits-------
boxlabs <- c("Terrestrial", "Epiphyte")

lma_lab <- expression(Leaf~Thickness~~(g~m^-2))

trtcols <- c("forestgreen","steelblue4")
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5))

jpeg(filename = "jobtalk/lma.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)

boxplot(sla_cm2g ~ niche2, data=sla_nohemi,xaxt='n',ylim=c(0, 235),
        boxlwd=2,whisklwd=2, staplelwd=2,
        ylab=lma_lab, outline=FALSE, border=trtcols ,col=trtcols2)
axis(1, boxlabs, at=1:2, cex.axis=1.25)
stripchart(sla_cm2g ~ niche2, data = sla_nohemi,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

dev.off()
