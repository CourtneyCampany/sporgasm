#vein density plots

niche <- read.csv("raw_data/species_niches.csv")
veins <- read.csv("raw_data/vein_density_lascruces.csv")

veinden <- merge(veins, niche, by=c("site", "genusspecies"))
  veinden$niche2 <- veinden$niche
  veinden$niche2 <- gsub("climber", "terrestrial", veinden$niche2)
  veinden$niche2 <- as.factor(veinden$niche2)
 
  #reorder from ground to canopy 
  veinden$niche2<-factor(veinden$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
vein_agg <- doBy::summaryBy(area_mm2 + vein_length_mm + vein_density_mm_mm2 ~
                            species + niche2 + plant_no, FUN=mean,
                            keep.names = TRUE, data=veinden)
  
#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")
  
gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

veinlab <- expression(Vein~Density~~(mm~mm^-2))

#vein length -----

# windows()

# jpeg(filename = "output/vein_length.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(vein_density_mm_mm2 ~ niche2, data=vein_agg, xaxt='n',varwidth=TRUE,
        ylab=veinlab,border=trtcols, ylim=c(0, 2.5))
axis(1, boxlabs, at=1:3, cex=1.1)
title(main= "partial Las Cruces only", line=-1)
stripchart(vein_density_mm_mm2 ~ niche2, data = vein_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

# dev.off()

