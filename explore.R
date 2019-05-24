source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")

##PCA using only means of traits, 
## pvcuurves are not from sample individuals as morphology stuff

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
epi <- alldata[alldata$niche2 == "epiphyte",]

alldata_mean <- doBy::summaryBy(. ~  species +
                             site + niche2, FUN=mean2, data=alldata, 
                             keep.names = TRUE)


plot(sla_cm2g  ~ stipe_length_cm, data=alldata, col=trtcols[niche2])
plot(sla_cm2g  ~ stipe_length_cm, data=epi, col=trtcols[3])
plot(sla_cm2g  ~ stipe_length_cm, data=alldata_mean, col=trtcols[3])

plot(waterpot_tlp ~ stipe_length_cm, data=alldata, col=trtcols[niche2])
plot(waterpot_tlp~ stipe_length_cm, data=epi, col=trtcols[3])

plot(stomatal_size~ stipe_length_cm, data=alldata, col=trtcols[niche2])
plot(stomatal_size~ stipe_length_cm, data=epi, col=trtcols[3])

plot(chl_mg_m2 ~ stipe_length_cm, data=alldata, col=trtcols[niche2])
plot(chl_mg_m2 ~ stipe_length_cm, data=epi, col=trtcols[3])
#no for stipe and pv curve parameters

##yes plots
library(magicaxis)
par(mgp=c(2,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#stipe length and xa (maybe scales differently, little to no stipe)
plot(xylem_area_mm2 ~ stipe_length_cm, data=alldata, col=trtcols2[niche2],
     xlab=stipe_lab, ylab=xylem_lab,pch=16,cex=1.25, ylim=c(0,1.1), xlim=c(0,80))
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)


#lamina area and xa (some outliers, if remove do they have same slope?)
with(alldata, plot(log10(lamina_area_cm2) ~ log10(xylem_area_mm2),
                     xlab=lamina_lab, ylab=xylem_lab,axes=FALSE,
                     pch=16, col=trtcols2[niche2],cex=1.25))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

#stipe length and stomata density
plot(sd_mm2~ stipe_length_cm, data=alldata, col=trtcols2[niche2],xlab=stipe_lab,
     ylab=sd_lab,pch=16,cex=1.25, ylim=c(0,150), xlim=c(0,80))
legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)


par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(sd_mm2 ~ xylem_area_mm2, data=hubersd, type='n', ylab=sd_lab,
     xlab=xylem_lab,xlim=c(0,1), ylim=c(0,150))
points(sd_mm2 ~ xylem_area_mm2, data=hubersd, 
       pch=16, col=trtcols2[niche2],cex=1.25)
smoothplot(xylem_area_mm2, sd_mm2, data=hubersd, kgam=5, R="species",
           linecol="black",pch="", add=TRUE)
