#testing trait correlations

source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

#huber by niche
huber <- read.csv("calculated_data/xylem_area_huber.csv")

boxplot(huber ~ niche2, data=huber,xaxt='n',
        ylab = "Huber values",border=trtcols, varwidth=TRUE, outline=FALSE)

#morphology traits
traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#stomata density
sd <- read.csv("calculated_data/stomata_density_means.csv")  
  
#pressure volume curves
pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)
  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  pv$plant_no <- as.integer(pv$plant_no)
  
sla <- read.csv("calculated_data/fern_sla.csv")
  #reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
kleaf <- read.csv("calculated_data/kleaf.csv")
#reorder from ground to canopy 
  kleaf$niche2<-factor(kleaf$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  names(kleaf)[2] <- "plant_no"
  
huberstipe <- merge(huber, traits[,c(1:2,4,6)])
huberleaf <- merge(huber, traits[,c(1:2,4,7)])
hubersd <- merge(huber, sd)
huberpv <- merge(huber, pv)
khuber <- merge(kleaf, huber)
ksd <- merge(kleaf, sd)

sdsla <- merge(sla,sd)
pvsla <- merge(pv, sla)
pvsd <- merge(pv, sd)


library(mgcv) 

pv_mean <- doBy::summaryBy(waterpot_tlp + osmotic_potential ~ genusspecies +
                           site + niche2, FUN=mean, data=pv, keep.names = TRUE)
kl_mean <- doBy::summaryBy(kmax_leaf +  K~ genusspecies +
                             site + niche2, FUN=mean, data=kleaf, keep.names = TRUE)
pvk <- merge(pv_mean,kl_mean)

#nope
plot(huber ~ stipe_length_cm, data=huberstipe[huberstipe$huber < .0005,])
plot(sd_mm2 ~ huber, data=hubersd[hubersd$huber < .0005,])
plot(waterpot_tlp ~ huber, data=huberpv[huberpv$huber < .0005,])
plot(osmotic_potential ~ huber, data=huberpv[huberpv$huber < .0005,])
plot(waterpot_tlp ~ xylem_area_mm2, data=huberpv)
plot(osmotic_potential ~ xylem_area_mm2, data=huberpv)
plot(sd_mm2 ~ sla_cm2g, data=sdsla, pch=16, col=trtcols2[niche2])
plot(waterpot_tlp ~ sla_cm2g, data=pvsla, pch=16, col=trtcols2[niche2])
plot(sd_mm2 ~ waterpot_tlp, data=pvsd, pch=16, col=trtcols2[niche2])
plot(sd_mm2 ~ osmotic_potential, data=pvsd, pch=16, col=trtcols2[niche2])
plot(sd_mm2 ~ elasticity, data=pvsd, pch=16, col=trtcols2[niche2])
plot(waterpot_tlp ~ K, data=pvk, pch=16, col=trtcols2[niche2])


plot(log10(lamina_area_cm2) ~ log10(xylem_area_mm2) , data=huberleaf, pch=16, col=trtcols2[niche2])

library(magicaxis)
par(mgp=c(2,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
with(huberleaf, plot(log10(lamina_area_cm2) ~ log10(xylem_area_mm2),
                    xlab=lamina_lab, ylab=xylem_lab,axes=FALSE,
                    pch=16, col=trtcols2[niche2],cex=1.25))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)



plot(K ~ sd_mm2, data=ksd, pch=16, col=trtcols2[niche2])

#maybe but stats say no
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(sd_mm2 ~ xylem_area_mm2, data=hubersd, type='n', ylab=sd_lab,
     xlab=xylem_lab,xlim=c(0,1), ylim=c(0,150))
points(sd_mm2 ~ xylem_area_mm2, data=hubersd, 
       pch=16, col=trtcols2[niche2],cex=1.25)
smoothplot(xylem_area_mm2, sd_mm2, data=hubersd, kgam=5, R="species",
           linecol="black",pch="", add=TRUE)

library(lme4)
library(car)
hubersd_mod <- lmer(sd_mm2 ~ xylem_area_mm2 * niche2 + (1|species),
                    data=hubersd)
Anova(hubersd_mod)
r.squaredGLMM(hubersd_mod)
##gg


