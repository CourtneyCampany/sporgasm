source("master_scripts/plot_objects.R")

leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "hemi-epiphyte", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)

#reorder from ground to canopy 
leafchem$niche2<-factor(leafchem$niche2, 
                 levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))
  
#nperc -----
cldnitro <- c("a","a","b" )

# windows()
# jpeg(filename = "output/nitrogen.jpeg",
#       width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(n_perc ~ niche2, data=leafchem, xaxt='n',ylim=c(0,6),border=trtcols,
        ylab="Foliar Nitrogen  (%)", varwidth=TRUE)
stripchart(n_perc ~ niche2, data = leafchem,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex=1.1)
text(x=1:3, y=5.5, cldnitro)

# dev.off()


