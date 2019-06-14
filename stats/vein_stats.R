
#vein density plots
niche <- read.csv("raw_data/species_niches.csv")
veins <- read.csv("raw_data/vein_density.csv")

veinden <- merge(veins, niche, by="genusspecies")
veinden$niche2 <- veinden$niche
veinden$niche2 <- gsub("climber", "hemi-epiphyte", veinden$niche2)
veinden$niche2 <- as.factor(veinden$niche2)

#reorder from ground to canopy 
veinden$niche2<-factor(veinden$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


### custom quantiles for paper
terr <- veinden[veinden$niche2 == "terrestrial",]
epi <- veinden[veinden$niche2 == "epiphyte",]
hemi <- veinden[veinden$niche2 == "hemi-epiphyte",]

# b20epi <- nrow(epi[epi$stipe_length_cm <= 20 ,])
# a20epi <- nrow(epi[epi$stipe_length_cm > 20 ,])
# 
# b20terr <- nrow(terr[terr$stipe_length_cm <= 20 ,])
# a20terr <- nrow(terr[terr$stipe_length_cm > 20 ,])
# 
# b20hemi <- nrow(hemi[hemi$stipe_length_cm <= 20 ,])
# a20hemi <- nrow(hemi[hemi$stipe_length_cm > 20 ,])

boxplot(vein_density_mm_mm2 ~ niche2, data=veinden)
#quite a few, lets see if the matter

## stats on basic stipe morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)
library(lme4)
library(MuMIn)

boxplot(vein_density_mm_mm2 ~ niche2, data=veinden) #outliers may be present
hist(veinden$vein_density_mm_mm2)

##full mixed model:
vd_mod <- lmer(sqrt(vein_density_mm_mm2) ~ niche2 * site
                   + (1|species), data=veinden)

vd_mod2 <- lmer(sqrt(vein_density_mm_mm2) ~ niche2 + site
               + (1|species), data=veinden)


plot(vd_mod)
qqPlot(residuals(vd_mod))

#model summary
Anova(vd_mod, type="3") #niche but no interaction
anova(vd_mod, vd_mod2) #not different
AIC(vd_mod, vd_mod2) #aic close, use no interaction

#use model without interaction

Anova(vd_mod2, type="3") #nothing
r.squaredGLMM(vd_mod2) #all species driven

#           R2m       R2c
#0.04825974 0.8682842




