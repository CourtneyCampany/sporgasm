#cholorophyll plots
source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")

traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category
traits$niche2 <- traits$niche
traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
traits$niche2 <- as.factor(traits$niche2)

chloro <- traits[complete.cases(traits$chl_mg_m2),]  

#reorder from ground to canopy 
chloro$niche2<-factor(chloro$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##same data as stats, drop outliers and bad data for elaher
chloro4 <- chloro[chloro$chl_mg_m2 < 800 & !chloro$genusspecies == "elaher",]


#drop oleart as we do in model
chlstomata <- c("ab","b","a" )
chllab <- expression(Chlorophyll~content~~(mg~m^-2))

# windows()

# jpeg(filename = "output/chlorophll.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=.9, cex.axis=.9)
boxplot(chl_mg_m2 ~ niche2, data=chloro4, xaxt='n',ylim=c(0, 900),
        varwidth=TRUE, ylab=chllab,border=trtcols,outline=FALSE,
        boxlwd=2,whisklwd=2,staplelwd=2, xlab="")
axis(1, boxlabs, at=1:3, cex.axis=.9)
stripchart(chl_mg_m2 ~ niche2, data = chloro4,cex=1,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=890, chlstomata, cex=.9)
# dev.off()
