
traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  
lamina <- traits[complete.cases(traits$lamina_area_cm2),]
  
## stats on basic leaf morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(outliers)
library(lme4)
library(MuMIn)

##full mixed model:
boxplot(lamina_area_cm2 ~ niche2, data=lamina) #outliers, but they were big

#variables are several orders of magnitude so log transformation
lamina_mod <- lmer(log10(lamina_area_cm2) ~ niche2 * site + (1|species), 
                   data=lamina)
lamina_mod2 <- lmer(log10(lamina_area_cm2) ~ niche2 + site + (1|species), 
                    data=lamina)

#model diagnostics
qqPlot(residuals(lamina_mod2)) #pretty good
plot(lamina_mod2) ##negative skewed so we need a transformation

#model summary
Anova(lamina_mod, type="3") #no interactions
anova(lamina_mod, lamina_mod2) #not different
AIC(lamina_mod, lamina_mod2)

#use model without interaction
summary(lamina_mod2)
Anova(lamina_mod2, type="3")
r.squaredGLMM(lamina_mod)
#R2m       R2c
#0.1286234 0.8875239

# no diff, related to species

visreg(lamina_mod2, trans=log)
##slightly higher SD at las cruces

tukey_la <- glht(lamina_mod2, linfct = mcp(niche2 = "Tukey"))
la_siglets <-cld(tukey_la)

#terrestrial hemi-epiphyte      epiphyte 
# "a"           "a"           "a"

