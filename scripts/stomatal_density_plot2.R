source("master_scripts/plot_objects.R")

stodens <- read.csv("calculated_data/stomata_density.csv")
  stodens$niche2 <- gsub("climber", "hemi-epiphyte", stodens$niche)
  stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + genusspecies,
                          data=stodens, FUN=mean, keep.names = TRUE)
# write.csv(sd_agg, "calculated_data/stomata_density_means.csv", row.names = FALSE)

#plot bits-------

#sd plot ----------
sd_new <- droplevels(sd_agg[!sd_agg$genusspecies == "oleart",])
#drop oleart as we do in model
cldstomata <- c("a","b","b" )

# windows()

# jpeg(filename = "output/stomatadensity.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(sd_mm2 ~ niche2, data=sd_new, xaxt='n',ylim=c(0, 162),varwidth=TRUE,
        ylab=expression(Stomatal~density~~(mm^2)),border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(sd_mm2 ~ niche2, data = sd_new,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=157, cldstomata)
# dev.off()
