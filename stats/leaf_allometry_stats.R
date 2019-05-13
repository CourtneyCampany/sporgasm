library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

## compare slopes of treatments with leaf allometry

traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "terrestrial", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


arealength <- lmer(log10(lamina_area_cm2) ~ log10(frond_length_cm)
                   * niche2 + (1|species), data=traits)
anova(arealength, type=3)
r.squaredGLMM(arealength)
# R2m       R2c
# [1,] 0.53615 0.9109152

al_slopes <- lstrends(arealength, "niche2", var="frond_length_cm")
pairs(al_slopes) #not different

arealength_mod2 <- sma(lamina_area_cm2 ~ frond_length_cm * niche2,
                       log="xy", data=traits, multcomp = TRUE,
                       multcompmethod='adjusted')
arealength_mod3 <- sma(lamina_area_cm2 ~ frond_length_cm + niche2,
                       log="xy", data=traits, multcomp = TRUE,
                       multcompmethod='adjusted')

arealength_mod4 <- sma(lamina_area_cm2 ~ frond_length_cm * niche2,
                       log="xy", data=traits) #common slope test
arealength_mod5 <- sma(lamina_area_cm2 ~ frond_length_cm + niche2,
                       log="xy", data=traits) #common elevation test

##slopes are equal but elevation between treatments is different
## elevation is different between hemi and epi, but rest are same



# H0 : slopes are equal.
# Likelihood ratio statistic : 0.135 with 2 degrees of freedom
# P-value : 0.93473 ###yes slopes are equal

