source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)
library(mgcv) #for gam fits

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

terr <- droplevels(stomata_noout[stomata_noout$niche2 == "terrestrial",])
hemi <- droplevels(stomata_noout[stomata_noout$niche2 == "hemi-epiphyte" ,])
epi <- droplevels(stomata_noout[stomata_noout$niche2 == "epiphyte",])

terr2 <- terr[complete.cases(terr$stomatal_size) & complete.cases(terr$sd_mm2),]
hemi2 <- hemi[complete.cases(hemi$stomatal_size) & complete.cases(hemi$sd_mm2),]
epi2 <- epi[complete.cases(epi$stomatal_size) & complete.cases(epi$sd_mm2),]

terr_mod <- lm(stomatal_size ~ sd_mm2 , data=terr)
hemi_mod <- lm(stomatal_size ~ sd_mm2 , data=hemi)
epi_mod <- lm(stomatal_size ~ sd_mm2 , data=epi)

fernmod <- lm(stomatal_size ~ sd_mm2, stomata_noout)


#loess for sd vs ss
# loess_terr <- loess(stomatal_size ~ sd_mm2, data=terr2, span=5)
# terr2$smooth_terr <- predict(loess_terr)
# 
# loess_hemi<- loess(stomatal_size ~ sd_mm2, data=hemi2, span=5)
# hemi2$smooth_hemi <- predict(loess_hemi)
# 
# loess_epi <- loess(stomatal_size ~ sd_mm2, data=epi2, span=5)
# epi2$smooth_epi2 <- predict(loess_epi)

##panel plot-------

# jpeg(filename = "output/figures2_stomata.jpeg",
#      width = 10, height = 6, units = "in", res= 400)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=.9, cex.axis=.9)

plot(stomatal_size ~ sd_mm2, data=stomata_noout,
     ylab=ss_lab, xlab=sd_lab, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(stomatal_size ~ sd_mm2, data=stomata_noout, pch=16, col= trtcols2[niche2],
       cex=1)
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01, cex=.9)

lines(x=terr2[order(terr2$sd_mm2),"sd_mm2"],
      y=terr2[order(terr2$sd_mm2),"smooth_terr"],
      lwd=4, lty=2, col=trtcols[1])
lines(x=hemi2[order(hemi2$sd_mm2),"sd_mm2"],
      y=hemi2[order(hemi2$sd_mm2),"smooth_hemi"],
      lwd=4, lty=2, col=trtcols[2])
lines(x=epi2[order(epi2$sd_mm2),"sd_mm2"],
      y=epi2[order(epi2$sd_mm2),"smooth_epi"],
      lwd=4, lty=2, col=trtcols[3])

text(110,2500, expression(paste(R[cond]^{"2"}," = "," 0.22")), cex=.9)
text(110,2150, expression(paste(R[marg]^{"2"}," = "," 0.90")), cex=.9)
# dev.off()
