source("functions_packages/basic_functions.R")

traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  
#reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                         levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

## stats on basic leaf morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(lme4)
library(MuMIn)
  
# frond_mod <- lm(frond_length_cm ~ niche2, data=traits)
# #model diagnostics
# frondqq <- qqPlot(residuals(frond_mod)) ##there are a couple of bad points
# # identify(frondqq) #not needed but for reference
# plot(frond_mod) #evidence to remove observation 226, 228 and 203
# 
# frond_mod2 <- lm(frond_length_cm ~ niche2, data=traits[-c(205, 206,226,228,203),])
#   plot(frond_mod2)
#   qqPlot(frond_mod2)
# 
#   Ltest_frond <- leveneTest(frond_length_cm ~ niche2 , data = traits)
#   summary(Ltest_frond) # signficant so variance are not equal
# 
# bwplot(frond_length_cm ~ niche2 | site , data = traits) 
# visreg(frond_mod2)  

# summary(frond_mod2)
# anova(frond_mod2)

# tukey_frond <- glht(frond_mod2, linfct = mcp(niche2 = "Tukey"))
# frond_siglets <-cld(tukey_frond)
# frond_siglets2 <- frond_siglets$mcletters$Letters
#terrestrial longer frond length than  epi, hemi same as epi


##linear mixed model with species as random (account for species variation)

boxplot(frond_length_cm ~ niche2, data=traits)
hist(traits$frond_length_cm)
#drop row 203

frond_dat <- traits[-203,]
hist(frond_dat$frond_length_cm)

frond_mod4 <- lmer(sqrt(frond_length_cm) ~ niche2 * site + (1|species), 
                    data=frond_dat)
# frond_mod5 <- lmer(sqrt(frond_length_cm) ~ niche2 + site + (1|species), 
#                    data=frond_dat)
  
# plot(frond_mod4)
# qqPlot(residuals(frond_mod4))
# 
# #model summary
# Anova(frond_mod4, type="3") #niche but no interaction
# anova(frond_mod4, frond_mod5) #not different
# AIC(frond_mod4, frond_mod5) #first model is  better

#use model without interaction
# summary(frond_mod4)
Anova(frond_mod4, type="3")
r.squaredGLMM(frond_mod4)

# R2m       R2c
# [1,] 0.1419035 0.8099322

#niche2 0.0332 *


visreg(frond_mod4)
##frond longer in terrestrial

tukey_fr <- glht(frond_mod4, linfct = mcp(niche2 = "Tukey"))
fr_siglets <-cld(tukey_fr)

# terrestrial hemi-epiphyte      epiphyte 
# "a"          "ab"           "b" 

terr <- mean(frond_dat[frond_dat$niche2 == "terrestrial", "frond_length_cm"], 
             na.rm=TRUE) #84.7887
terr_se <- se(frond_dat[frond_dat$niche2 == "terrestrial", "frond_length_cm"])


epi <- mean(frond_dat[frond_dat$niche2 == "epiphyte", "frond_length_cm"], 
               na.rm=TRUE) #58.94787
epi_se <- se(frond_dat[frond_dat$niche2 == "epiphyte", "frond_length_cm"])
