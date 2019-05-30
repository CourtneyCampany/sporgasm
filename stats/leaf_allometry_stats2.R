library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

## compare slopes of treatments with leaf allometry (stipe vs lamina)

traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
traits$stipe_nozero <- traits$stipe_length_cm + .1


arealength <- lmer(log10(lamina_area_cm2) ~ log10(stipe_nozero)
                   * niche2  + (1|species), data=traits)
anova(arealength, type=3)
r.squaredGLMM(arealength)
# R2m       R2c
# [1,] 0.2268914 0.8813653

al_slopes <- lstrends(arealength, "niche2", var="stipe_nozero")
pairs(al_slopes) #terresrial and epi diff, terr marginally different than hemi

arealength_mod2 <- sma(lamina_area_cm2 ~ stipe_nozero * niche2,
                       log="xy", data=traits, multcomp = TRUE,
                       multcompmethod='adjusted') #slopes not equal
summary(arealength_mod2)

arealength_mod3 <- sma(lamina_area_cm2 ~ stipe_nozero + niche2,
                       log="xy", data=traits, multcomp = TRUE,
                       multcompmethod='adjusted') #elevation is different
summary(arealength_mod3)

arealength_mod4 <- sma(lamina_area_cm2 ~ stipe_nozero * niche2,
                       log="xy", data=traits) #common slope test
summary(arealength_mod4)

arealength_mod5 <- sma(lamina_area_cm2 ~ stipe_nozero + niche2,
                       log="xy", data=traits) #common elevation test
summary(arealength_mod5)

#slopes and elevation are different
#traits correlated within groups, weakest with epiphytes




# H0 : slopes are equal.
# Likelihood ratio statistic : 0.135 with 2 degrees of freedom
# P-value : 0.93473 ###yes slopes are equal

