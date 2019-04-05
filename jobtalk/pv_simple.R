## pv curve figures


pv <- read.csv("calculated_data/pv_curves.csv")
  pv$niche2 <- gsub("climber", "terrestrial", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)

  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
##only plot las cruces until las selva is double checked
cruces <- pv[pv$site == "las_cruces",]

cruces_nohemi <- droplevels(cruces[!cruces$niche2 == "hemi-epiphyte",])
  
#plot bits-------
boxlabs <- c("Terrestrial", "Epiphyte")

trtcols <- c("forestgreen","steelblue4")
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5))

#labels
tlp_lab <- expression(paste(Pi[tlp], "  (MPa)"))

           
## waterpotenail turgor loss point 

jpeg(filename = "jobtalk/turgorlosspoint.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(waterpot_tlp ~ niche2, data=cruces_nohemi, xaxt='n',ylim=c(-2,0.1),
        ylab=tlp_lab,boxlwd=2,whisklwd=2, staplelwd=2,
        col=trtcols2)
axis(1, boxlabs, at=1:2, cex.axis=1.25)
stripchart(waterpot_tlp ~ niche2, data = cruces_nohemi,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols, xaxt='n', add=TRUE)
dev.off()
