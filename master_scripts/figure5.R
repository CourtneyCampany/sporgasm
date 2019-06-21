## pv curve figures
source("master_scripts/plot_objects.R")

pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)

  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

  
#plot bits-------

op_lab <- expression(paste(Psi[S], "  (MPa)"))
elasticity_lab <- expression(paste(epsilon, "  (MPa)")) 
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))
cap_lab <- "Tissue Capacitance  (units?)"

## osmotic potential

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(osmotic_potential ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab=op_lab,ylim=c(-2,0.1),
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(osmotic_potential ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
           
           
## water potential turgor loss point 
# jpeg(filename = "output/tlp.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(waterpot_tlp ~ niche2, data=pv, xaxt='n',ylim=c(-2,0.1),
        varwidth=TRUE,ylab=tlp_lab,
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(waterpot_tlp ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
# dev.off()

## elasticity

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(elasticity ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab=elasticity_lab,
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(elasticity ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

## capacitance full

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(capacitance_full ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab="Tissue Capacitance full  (units?)",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(capacitance_full ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

## RWC @TLP
          
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(rwc_tlp ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab="RWC @ TLP",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(rwc_tlp ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 