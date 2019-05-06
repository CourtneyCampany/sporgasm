## stats on basic leaf morphology-------
library(visreg)
library(lme4)
library(MuMIn)
library(multcomp)
library(car)
library(lattice)
library(outliers)

kleaf <- read.csv("calculated_data/kleaf.csv")

#reorder from ground to canopy 
kleaf$niche2<-factor(kleaf$niche2, 
              levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


#basic stats
hist(kleaf$kmax_leaf)
boxplot(kmax_leaf ~ niche2, data=kleaf) #couple of big outlers

kleaf_clean <- kleaf[kleaf$kmax_leaf < .025,]

hist(kleaf_clean$kmax_leaf)
boxplot(kmax_leaf ~ niche2, data=kleaf_clean)

#simple model check
kl_mod <- lm(sqrt(kmax_leaf) ~ niche2, data=kleaf_clean)
#model diagnostics
qqPlot(residuals(kl_mod)) #pretty good
residualPlot(kl_mod)



##full mixed model:
kl_mod2 <- lmer(sqrt(kmax_leaf) ~ niche2 * site + (1|species), data=kleaf_clean)
kl_mod3 <- lmer(sqrt(kmax_leaf) ~ niche2 + site + (1|species), data=kleaf_clean)

#model summary
Anova(kl_mod2, type="3") #nothing
AIC(kl_mod2, kl_mod3) #model 2 is  better 

#use model with interaction
r.squaredGLMM(kl_mod2)
#R2m       R2c
#0.2002133 0.8036984



