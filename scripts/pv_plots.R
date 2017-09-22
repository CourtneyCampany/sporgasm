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


boxplot(waterpot_tlp ~ niche, data=hydro_agg,xaxt='n', ylim=c(-1.5, -.5),
        ylab=("Water Potential @\n Turgor Loss Point"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(rwc_tlp ~ niche, data=hydro_agg,xaxt='n', ylim=c(80, 100),
        ylab=("Relative Water Content @\n Turgor Loss Point"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(elasticity ~ niche, data=hydro_agg,xaxt='n',ylim=c(0, 40),
        ylab=("Modulus of Elasticity @\n Full Furgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(capacitance_full ~ niche, data=hydro_agg,xaxt='n',ylim=c(0,.18),
        ylab=("Relative Capacitance @\n Full Furgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(capacitance_zero. ~ niche, data=hydro_agg,xaxt='n',ylim=c(0,1),
        ylab=("Relative Capacitance @\n Zero Turgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(capacitance_absolute ~ niche, data=hydro_agg,xaxt='n', ylim=c(0,.5),
        ylab=("Absolute Capacitance per\n Leaf Area @ Full Turgor"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)

boxplot(SWC ~ niche, data=hydro_agg,xaxt='n',ylim=c(0, 8),
        ylab=("Saturated Water Content"), outline=FALSE)
axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
