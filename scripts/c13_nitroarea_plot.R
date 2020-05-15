source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <- with(alldata, 1/(sla_cm2g/10000)) #g m-2
alldata$nitro_area <- with(alldata,lma * (n_perc/100))

terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte",]
epi <- alldata[alldata$niche2 == "epiphyte",]

#plotting

terr_mod <- lm(d13C ~ nitro_area, data=terr)
hemi_mod <- lm(d13C ~ nitro_area, data=hemi)
epi_mod <- lm(d13C ~ nitro_area, data=epi)

windows()
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

plot(d13C ~ nitro_area , data=alldata, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(d13C ~ nitro_area,data=alldata,pch=16,  col= trtcols2[niche2],
       cex=1.25)
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
