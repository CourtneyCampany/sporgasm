#stipe bivariate plots

source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)

# plot(lamina_area_cm2~ stipe_length_cm, data=alldata, xlab=stipe_lab,
#      ylab=sd_lab,  col=trtcols2[niche2],
#      pch=16,cex=1.25)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=terr)
hemi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=hemi)
epi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=epi)

terr_mod_sd <- lm(sd_mm2~ stipe_length_cm, data=terr)
hemi_mod_sd<- lm(sd_mm2 ~ stipe_length_cm, data=hemi)
epi_mod_sd<- lm(sd_mm2 ~ stipe_length_cm, data=epi)

#plot bits -----

# jpeg(filename = "output/figure3.jpeg",
#      width = 12, height = 5, units = "in", res= 400)  

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#stipe length and xa (maybe scales differently, little to no stipe)
plot(xylem_area_mm2 ~ stipe_length_cm, data=alldata, type='n',
     xlab=stipe_lab, ylab=xylem_lab, ylim=c(0,1.1), xlim=c(0,80))
predline(terr_mod_xa, col=trtcols[1], lwd=2, lty=2)
predline(epi_mod_xa, col=trtcols[3], lwd=2, lty=2)
points(xylem_area_mm2 ~ stipe_length_cm, data=alldata, col=trtcols2[niche2],
     pch=16,cex=1.25)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)


#stipe length and stomata density
plot(sd_mm2~ stipe_length_cm, data=alldata, xlab=stipe_lab,type='n',
     ylab=sd_lab, ylim=c(0,150), xlim=c(0,80))
predline(terr_mod_sd, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod_sd, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod_sd, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stipe_length_cm, data=alldata, col=trtcols2[niche2],
       pch=16,cex=1.25)
# legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

# dev.off()
