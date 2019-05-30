
traits <- read.csv("calculated_data/fern_traits.csv")

  ## Climber seems to be close to terrestrial and it still is technically-------
  ## create new variable that adds climber to terrestrial category
  traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)

chloro <- traits[complete.cases(traits$chl_mg_m2),]  

#reorder from ground to canopy 
chloro$niche2<-factor(chloro$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

# chl_mod <- lm(chl_mg_m2 ~ niche2, data=chloro)
# 
# #model diagnostics
# qqPlot(chl_mod) 
# plot(chl_mod) #looks pretty good
# skewness(chl_mod$residuals) #less than 1
# kurtosis(chl_mod$residuals) 
# hist(chl_mod$residuals)
# 
# summary(chl_mod)
# anova(chl_mod) #broad differences in niche
# 
# Ltest_frond <- leveneTest(chl_mg_m2 ~ niche2 , data = chloro)
# Ltest_frond ##all good


##linear mixed model with species as random --------
library(lme4)
library(MuMIn)
library(arm)


boxplot(chl_mg_m2 ~ niche2, data=chloro) #couple of outlers
#if keep outliers in nothing is significant, but epi looks lower
#outler for epi is one species, evidence to maybe delete (too much variation)
#or chlo < 800 for all outliers

### I have removed elaher as we dont know what is real value and the 2 outliers

chloro2 <- chloro[!chloro$genusspecies == "elaher",]
chloro3 <- chloro[chloro$chl_mg_m2 < 800,]
chloro4 <- chloro[chloro$chl_mg_m2 < 800 & !chloro$genusspecies == "elaher",]
boxplot(chl_mg_m2 ~ niche2, data=chloro4)

#models
chl_mod3 <- lmer(chl_mg_m2 ~ niche2 * site + (1|species), data=chloro4)
chl_mod4 <- lmer(chl_mg_m2 ~ niche2 + site + (1|species), data=chloro4)

hist(chloro4$chl_mg_m2) #look good
plot(chl_mod3) #not bad
qqPlot(residuals(chl_mod3)) #not bad

#model summary
Anova(chl_mod3, type="3") #niche effect
anova(chl_mod3, chl_mod4) #not different
AIC(chl_mod3, chl_mod4) 

#AIC way better,keep interaction
summary(chl_mod3)
Anova(chl_mod3, type="3")
r.squaredGLMM(chl_mod3)
#R2m       R2c
#0.1261207 0.5984076
# niche2    0.03146 * 

visreg(chl_mod3)
##slightly higher chat las cruces

tukey_chl <- glht(chl_mod3, linfct = mcp(niche2 = "Tukey"))
chl_siglets <-cld(tukey_chl)

# terrestrial hemi-epiphyte      epiphyte 
# "ab"           "b"           "a" 

epi <- mean(chloro4[chloro4$niche2 == "epiphyte", "chl_mg_m2"])
#390.9373
hemi <- mean(chloro4[chloro4$niche2 == "hemi-epiphyte", "chl_mg_m2"])
#536.0926