## frond length by lamina area

source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "terrestrial", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

terr <- traits[traits$niche2 == "terrestrial" & 
                 complete.cases(traits$frond_length_cm),]
hemi <- traits[traits$niche2 == "hemi-epiphyte" &
                 complete.cases(traits$frond_length_cm),]
epi <- traits[traits$niche2 == "epiphyte"&
                complete.cases(traits$frond_length_cm),]

#log model fits for allometry
terr_mod <- lm(log10(lamina_area_cm2) ~ log10(frond_length_cm) ,data=terr)

hemi_mod <- lm(log10(lamina_area_cm2) ~ log10(frond_length_cm) , data=hemi)

epi_mod <- lm(log10(lamina_area_cm2) ~ log10(frond_length_cm) ,data=epi)

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

fronddat <- traits[-203,] #same as stats

library(magicaxis)
library(plotrix)

##allometry figure
# jpeg(filename = "output/arealength.jpeg",
#       width = 7, height = 7, units = "in", res= 400) 

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
with(fronddat, plot(log10(lamina_area_cm2) ~ log10(frond_length_cm),
                    xlab=frond_lab, ylab=lamina_lab,axes=FALSE,
                    pch=21, bg=trtcols2[niche2],cex=1.25),
                    xlim=c(0,3))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("topleft", legend = boxlabs, pch=21, pt.bg=trtcols, bty="n", inset=.01)
ablineclip(terr_mod, x1=log10(min(terr$frond_length_cm)), 
           x2=log10(max(193.7)),
           col=trtcols[1], lwd=3, lty=2)
ablineclip(hemi_mod, x1=log10(min(hemi$frond_length_cm)), 
           x2=log10(max(hemi$frond_length_cm)),
           col=trtcols[2], lwd=3, lty=2)
ablineclip(epi_mod, x1=log10(min(epi$frond_length_cm)), 
           x2=log10(max(epi$frond_length_cm)),
           col=trtcols[3], lwd=3, lty=2)
# dev.off()
