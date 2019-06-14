source("master_scripts/plot_objects.R")

#vein density plots
niche <- read.csv("raw_data/species_niches.csv")
veins <- read.csv("raw_data/vein_density.csv")

veinden <- merge(veins, niche, by="genusspecies")
  veinden$niche2 <- veinden$niche
  veinden$niche2 <- gsub("climber", "hemi-epiphyte", veinden$niche2)
  veinden$niche2 <- as.factor(veinden$niche2)
 
  #reorder from ground to canopy 
  veinden$niche2<-factor(veinden$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  

windows()

# jpeg(filename = "output/vein_density.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(vein_density_mm_mm2 ~ niche2, data=veinden, xaxt='n',varwidth=TRUE,
        ylab=veinlab,border=trtcols,ylim=c(0,3),xlab="",outline=FALSE,
        boxlwd=2, whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(vein_density_mm_mm2 ~ niche2, data = veinden,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

# dev.off()

