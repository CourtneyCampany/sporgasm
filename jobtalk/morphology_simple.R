source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche)
  traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
  traits_nohemi <- droplevels(traits[!traits$niche2 == "hemi-epiphyte",])
  
#plot bits-------
  boxlabs <- c("Terrestrial", "Epiphyte")
  
  trtcols <- c("forestgreen","steelblue4")
  library(scales)
  library(doBy)
  trtcols2 <- c(alpha(trtcols[1], .4), alpha(trtcols[2], .4))


#lamina area ----------
  lamina_lab2 <- expression(Leaf~area~~(cm^2))

jpeg(filename = "jobtalk/laminasize.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)

boxplot(lamina_area_cm2 ~ niche2, data=traits_nohemi, ylim=c(0, 2300),xaxt='n',
        boxlwd=2,whisklwd=2, staplelwd=2,
        ylab=lamina_lab2 ,col=trtcols2, outline=FALSE)
axis(1, boxlabs, at=1:2, cex.axis=1.25)
stripchart(lamina_area_cm2 ~ niche2, data = traits_nohemi,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols, xaxt='n', add=TRUE)

dev.off()


# stipe length -------

jpeg(filename = "jobtalk/stipe.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)
boxplot(stipe_length_cm ~ niche2, data=traits_nohemi, ylim=c(0, 82),xaxt='n',
        boxlwd=2,whisklwd=2, staplelwd=2,
        ylab = stipe_lab,col=trtcols2, outline=FALSE)
axis(1, boxlabs, at=1:2, cex.axis=1.25)
stripchart(stipe_length_cm ~ niche2, data = traits_nohemi,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols, xaxt='n', add=TRUE)

dev.off()

