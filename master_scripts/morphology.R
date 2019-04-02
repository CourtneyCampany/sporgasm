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


#lamina area ----------
##all same (depends on model)

jpeg(filename = "output/leaffrond.jpeg",
     width = 7, height = 7, units = "in", res= 400)  

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)

boxplot(lamina_area_cm2 ~ niche2, data=traits, ylim=c(0, 2300),xaxt='n',
        ylab=lamina_lab, border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(lamina_area_cm2 ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

dev.off()


# stipe length -------
stipecld <- c("a", "b", "b")

jpeg(filename = "output/stipe.jpeg",
     width = 7, height = 7, units = "in", res= 400)  

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 80),xaxt='n',
        ylab = stipe_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(stipe_length_cm ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=78, stipecld)

dev.off()

# frond length ------

jpeg(filename = "output/frond_length.jpeg", width = 7, height = 7, 
     units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(frond_length_cm ~ niche2, data=traits, ylim=c(0, 185),xaxt='n',
        ylab = frond_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(frond_length_cm ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

dev.off()








##bivatiate
trtcols3 <- scales::alpha(trtcols,.8)

par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))

jpeg(filename = "output/leaffrond.jpeg",
     width = 7, height = 7, units = "in", res= 400)  

plot(lamina_area_cm2 ~ frond_length_cm , pch=21, bg=trtcols3[niche2], 
     xlab=frond_lab, ylab= lamina_lab, cex=1.25,
     data=traits[traits$frond_length_cm < 1000,])
legend("bottomright", legend = boxlabs, pch=21, pt.bg=trtcols, bty="n")
dev.off()
