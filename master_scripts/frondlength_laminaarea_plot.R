## frond length by lamina area

source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "terrestrial", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

fronddat <- traits[-203,] #same as stats


##bivariate
jpeg(filename = "output/arealength.jpeg",
     width = 7, height = 7, units = "in", res= 400) 

# windows()
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(lamina_area_cm2 ~ frond_length_cm , pch=21, bg=trtcols2[niche2], 
     xlab=frond_lab, ylab= lamina_lab, cex=1.25,
     data=fronddat)
legend("topleft", legend = boxlabs, pch=21, pt.bg=trtcols, bty="n", inset=.01)

dev.off()
