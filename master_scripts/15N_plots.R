source("master_scripts/plot_objects.R")

leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
leafchem$niche2 <- gsub("climber", "terrestrial", leafchem$niche)
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


#15N

# windows()

# jpeg(filename = "output/n15.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(d15N ~ site, data=leafchem, xaxt='n',ylim=c(-6.5,6.5),varwidth=TRUE,
        ylab=n15lab, outline=FALSE)
axis(1, c("La Selva","Las Cruces"), at=1:2, cex=1.1)
stripchart(d15N ~ site, data = leafchem,
           vertical = TRUE, method = "jitter",
           pch = 16, col= alpha("grey", .5), xaxt='n', add=TRUE) 
text(x=1:2, y=6.2, c("a", "b"))
# dev.off()
