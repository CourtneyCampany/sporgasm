source("master_scripts/plot_objects.R")

stodens <- read.csv("calculated_data/stomata_density.csv")
stodens$niche2 <- gsub("climber", "terrestrial", stodens$niche)
stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + genusspecies,
                          data=stodens, FUN=mean, keep.names = TRUE)

sd_agg_nohemi <- droplevels(sd_agg[!sd_agg$niche2 == "hemi-epiphyte",])

#plot bits-------
boxlabs <- c("Terrestrial", "Epiphyte")

trtcols <- c("forestgreen","steelblue4")
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .4), alpha(trtcols[2], .4))

#sd plot ----------
sd_new <- droplevels(sd_agg_nohemi[!sd_agg_nohemi$genusspecies == "oleart",])

# windows()

jpeg(filename = "jobtalk/stomatadensity.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(sd_mm2 ~ niche2, data=sd_new, xaxt='n',ylim=c(0, 162),
        boxlwd=2,whisklwd=2, staplelwd=2,
        ylab=expression(Stomatal~density~~(mm^2)),col=trtcols2)
axis(1, boxlabs, at=1:2, cex.axis=1.25)
stripchart(sd_mm2 ~ niche2, data = sd_new,cex=1.5,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

dev.off()
