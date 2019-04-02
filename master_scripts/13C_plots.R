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


#13c -----

#some outliers removed in model, so reflect that here
c13dat <- leafchem[!leafchem$genusspecies == "bleschi",]
cldc13 <- c("a","a","b" )
cld2c13 <- c("a","b" )
# windows()

# jpeg(filename = "output/c13.jpeg",
#      width = 10, height = 7, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,1,0),oma=c(4,4,1,1), cex.lab=1)

par(mar=c(0,0,0,0))
boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-40, -25),
        ylab="",border=trtcols,  varwidth=TRUE, outline=FALSE)
stripchart(d13C ~ niche2, data = c13dat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex=1.1)
text(x=1:3, y=-26, cldc13)
mtext(side=2, at=-32.5, line=2.5,text=c13lab, xpd=TRUE, las=3, cex=.9)

par(mar=c(0,0,0,0))
boxplot(d13C ~ site, data=c13dat, xaxt='n',ylim=c(-40, -25),
        ylab="",varwidth=TRUE, outline=FALSE, yaxt='n')
stripchart(d13C ~ site, data = c13dat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= alpha("grey", .5), xaxt='n', add=TRUE) 
axis(1, c("La Selva","Las Cruces"), at=1:2, cex=1.1)
axis(2, labels=FALSE, tick=TRUE, tcl=0.25)
axis(2, labels=FALSE, tick=TRUE, tcl=-0.25)
text(x=1:2, y=-26, cld2c13)

# dev.off()

