#stipe bivariate plots

source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(visreg)
library(plotrix)

##PCA using only means of traits, 
## pvcuurves are not from sample individuals as morphology stuff

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <-  with(alldata, 1/(sla_cm2g/10000))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#models
terr_mod_13c <- lm(d13C ~ lma, data=terr)
hemi_mod_13c <- lm(d13C ~ lma, data=hemi)
epi_mod_13c <- lm(d13C ~ lma, data=epi) #no epi

#simple models of stomata
#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

terr_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "terrestrial",])
hemi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "hemi-epiphyte",])
epi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "epiphyte",])

stom_mod <- lm(sd_mm2 ~ stomatal_size * niche2 * site, data=stomata_noout)


c13dat <- alldata[!alldata$genusspecies == "bleschi",]
cldc13 <- c("a","a","b") 

#plot
jpeg(filename = "output/figure2new.jpeg",
     width = 12, height = 5, units = "in", res= 400)

par(mfrow=c(1,3),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#stomata
plot(sd_mm2 ~ stomatal_size , data=stomata_noout,ylim=c(0,145),
     xlim=c(0.0003, .0029), xlab=ss_lab, ylab=sd_lab, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stomatal_size,data=stomata_noout,pch=16,  col= trtcols2[niche2])
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

#c13
boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-40, -25),boxlwd=2,
        whisklwd=2,staplelwd=2,
        ylab=c13lab,border=trtcols,  varwidth=TRUE, outline=FALSE)
stripchart(d13C ~ niche2, data = c13dat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex.axis=1.1)
text(x=1:3, y=-25.5, cldc13)


plot(d13C ~ lma, data=alldata, type='n',xlim=c(0, 550), ylim=c(-40, -26),
     xlab=lmalab, ylab=c13lab)
predline(terr_mod_13c, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod_13c, col=trtcols[3], lwd=2, lty=2)
points(d13C ~ lma, data=alldata, col=trtcols2[niche2],
       pch=16,cex=1.25)
legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

dev.off()
