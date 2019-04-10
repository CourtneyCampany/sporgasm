source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
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
##all same  ##depending on model there is a marginal effect of site

# jpeg(filename = "output/laminasize.jpeg",
#      width = 7, height = 7, units = "in", res= 400)  

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)

boxplot(lamina_area_cm2 ~ niche2, data=traits, ylim=c(0, 2300),xaxt='n',
        ylab=lamina_lab, border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(lamina_area_cm2 ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

# dev.off()


# stipe length -------
stipecld <- c("a", "b", "b")

# jpeg(filename = "output/stipe.jpeg",
#      width = 7, height = 7, units = "in", res= 400)  

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 82),xaxt='n',
        ylab = stipe_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(stipe_length_cm ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=80, stipecld)

# dev.off()

# frond length ------
frondcld <- c("a" ,"ab" , "b" )
fronddat <- traits[-203,] #same as stats

# jpeg(filename = "output/frond_length.jpeg", width = 7, height = 7, 
#      units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)
boxplot(frond_length_cm ~ niche2, data=fronddat, ylim=c(0, 215),xaxt='n',
        ylab = frond_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(frond_length_cm ~ niche2, data = fronddat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=210, frondcld)

# dev.off()


## morphology panel plot ------

# windows(9,12)

# jpeg(filename = "output/morphology.jpeg", width = 9, height = 12, 
#      units = "in", res= 400)

# par(mgp=c(2.5,1,0), oma=c(5,5,1,1), cex.lab=1, mfrow=c(3,1))
# 
# par(mar=c(0,0,0,0))
# boxplot(lamina_area_cm2 ~ niche2, data=traits, ylim=c(0, 2300),xaxt='n',
#         ylab="", border=trtcols, varwidth=TRUE, outline=FALSE)
# axis(1, labels=FALSE, tick=TRUE)
# stripchart(lamina_area_cm2 ~ niche2, data = traits,
#            vertical = TRUE, method = "jitter",
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
# mtext(side=2, at=1150, line=2.5,text=lamina_lab, xpd=TRUE, las=3, cex=.8)
# 
# par(mar=c(0,0,0,0))
# boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 82),xaxt='n',
#         ylab = "",border=trtcols, varwidth=TRUE, outline=FALSE)
# axis(1, labels=FALSE, tick=TRUE)
# stripchart(stipe_length_cm ~ niche2, data = traits,
#            vertical = TRUE, method = "jitter",
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
# text(x=1:3, y=80, stipecld, font=2)
# mtext(side=2, at=41, line=2.5,text=stipe_lab, xpd=TRUE, las=3, cex=.8)
# 
# par(mar=c(0,0,0,0))
# boxplot(frond_length_cm ~ niche2, data=fronddat, ylim=c(0, 215),xaxt='n',
#         ylab = "",border=trtcols, varwidth=TRUE, outline=FALSE)
# axis(1, boxlabs, at=1:3, cex=1.1)
# stripchart(frond_length_cm ~ niche2, data = fronddat,
#            vertical = TRUE, method = "jitter",
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
# text(x=1:3, y=210, frondcld, font=2)
# mtext(side=2, at=107.5, line=2.5,text=frond_lab, xpd=TRUE, las=3, cex=.8)

# dev.off()


