source("master_scripts/plot_objects.R")
#read in pv curve data

pv <- read.csv("raw_data/pv_traits.csv")
  pv$date <- as.Date(pv$date, format = "%d/%m/%Y", tz = "UTC")

hydraulics <- pv[complete.cases(pv$elasticity),]
hydraulics_noclimb <- droplevels(hydraulics[!hydraulics$niche == "climber",])


#have full set of values, lets take the mean by species for now
library(doBy)

hydro_agg <- summaryBy(. ~ species+  niche, data=hydraulics_noclimb, 
                       FUN=mean, keep.names = TRUE)


par(mgp=c(2.5,1,0), mar=c(5,5,1,1), cex.lab=1.1)
boxplot(osmotic_potential ~ niche, data=hydro_agg,xaxt='n',ylim=c(-1.25, -.5),
        ylab=("Osmotic Potential"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=-.55, labels=niche_lab2[2:4])


boxplot(waterpot_tlp ~ niche, data=hydro_agg,xaxt='n', ylim=c(-1.5, -.5),
        ylab=("Water Potential @\n Turgor Loss Point"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=-.6, labels=niche_lab2[2:4])

boxplot(rwc_tlp ~ niche, data=hydro_agg,xaxt='n', ylim=c(80, 105),
        ylab=("Relative Water Content @\n Turgor Loss Point"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=102.5, labels=niche_lab2[2:4])

boxplot(elasticity ~ niche, data=hydro_agg,xaxt='n',ylim=c(0, 40),
        ylab=("Modulus of Elasticity @\n Full Furgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=35, labels=niche_lab2[2:4])

boxplot(capacitance_full ~ niche, data=hydro_agg,xaxt='n',ylim=c(0,.19),
        ylab=("Relative Capacitance @\n Full Furgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=.18, labels=niche_lab2[2:4])

boxplot(capacitance_zero. ~ niche, data=hydro_agg,xaxt='n',ylim=c(0,1),
        ylab=("Relative Capacitance @\n Zero Turgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=.9, labels=niche_lab2[2:4])

boxplot(capacitance_absolute ~ niche, data=hydro_agg,xaxt='n', ylim=c(0,.5),
        ylab=("Absolute Capacitance per\n Leaf Area @ Full Turgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=.45, labels=niche_lab2[2:4])

boxplot(SWC ~ niche, data=hydro_agg,xaxt='n',ylim=c(0, 8),
        ylab=("Saturated Water Content"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
text(x=c(1,2,3,4),y=7, labels=niche_lab2[2:4])
