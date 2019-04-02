## pv curve figures


pv <- read.csv("calculated_data/pv_curves.csv")
  pv$niche2 <- gsub("climber", "terrestrial", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)

  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

  
#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")
  
gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))
  

## osmotic potential

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(osmotic_potential ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab="Osmotic Potential",ylim=c(-2,0.1),
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(osmotic_potential ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
           
           
## waterpotenail turgor loss point 

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(waterpot_tlp ~ niche2, data=pv, xaxt='n',ylim=c(-2,0.1),
        varwidth=TRUE,ylab="Water Potential TLP",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(waterpot_tlp ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

## elasticity

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(elasticity ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab="Elasticity",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(elasticity ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

## capacitance full

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(capacitance_full ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab="Capacitance full",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(capacitance_full ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
           