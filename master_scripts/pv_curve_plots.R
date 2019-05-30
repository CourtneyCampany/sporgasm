source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

library(mgcv) #for gam fits
library(scales)

#pv parameters
pv <- read.csv("calculated_data/pv_curves2.csv")
    pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
    pv$niche2 <- as.factor(pv$niche2)
    #reorder from ground to canopy 
    pv$niche2<-factor(pv$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##raw data points
pv_data <- read.csv("calculated_data/pv_curves_full.csv")
pv_data$niche2<-factor(pv_data$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##plotting curves
psi2lab <- expression(1/Psi~~(MPa^-1))
op_lab <- expression(paste(Psi[o], "  (MPa)"))
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))

## trim weird data
pv_data2 <- pv_data[pv_data$rwc_100 < 15 & pv_data$rwc_100 >= 0,]
pv_data3 <- pv_data[pv_data$rwc_100 < 10 & pv_data$rwc_100 >= 0 &
                      pv_data$psi2 < 40,]

terr <- pv_data3[pv_data3$niche2 == "terrestrial",]
hemi <- pv_data3[pv_data3$niche2 == "hemi-epiphyte",]
epi <- pv_data3[pv_data3$niche2 == "epiphyte",]

#clean up data:
pv_data4 <- pv_data3[complete.cases(pv_data3$niche2),]
abs <-  pv[pv$capacitance_absolute < .4,]

tlp_cld <- c("a","a","a" )
op_cld <- c("a","a","b" )
cap_cld <- c("a","ab","b" )

# jpeg(filename = "output/plot_4_pvparams.jpeg",
#      width = 12, height = 6, units = "in", res= 400)

par(oma=c(4,4,1,1), mfrow=c(1,3),mgp=c(2.5,.75,0),cex.lab=1.1)

#tlp
boxplot(waterpot_tlp ~ niche2, data=pv, xaxt='n',ylim=c(-2,0.1),
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
        varwidth=TRUE,ylab=tlp_lab,
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(waterpot_tlp ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=0, tlp_cld)
text(3.5, -2, "A", cex=1.25)


## osmotic potential
boxplot(osmotic_potential ~ niche2, data=pv, xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
        varwidth=TRUE,ylab=op_lab,ylim=c(-2,0.1),
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(osmotic_potential ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=0, op_cld)
text(3.5, -2, "B", cex=1.25)

#capacitance

boxplot(capacitance_zero. ~ niche2, data=abs, xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,ylim=c(0, 1.3),
        varwidth=TRUE,ylab=cap_lab,outline=FALSE,xlab="",
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(capacitance_zero. ~ niche2, data = abs,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=1.2, cap_cld)
text(3.5, 0, "C", cex=1.25)

# dev.off()
