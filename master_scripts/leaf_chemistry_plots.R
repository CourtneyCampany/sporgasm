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


#nperc -----

# windows()
jpeg(filename = "output/nitrogen.jpeg",
      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(n_perc ~ niche2, data=leafchem, xaxt='n',ylim=c(0,6),
        ylab="Foliar Nitrogen  (%)",col=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)

dev.off()


#13c -----

# windows()

jpeg(filename = "output/c13.jpeg",
      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(d13C ~ niche2, data=leafchem, xaxt='n',ylim=c(-40, -25),
        ylab=c13lab,col=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)

dev.off()

#15N

# windows()

 jpeg(filename = "output/n15.jpeg",
      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(d15N ~ niche2, data=leafchem, xaxt='n',ylim=c(-6,6),
        ylab=n15lab,col=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)

dev.off()
