
traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)

## stats on basic leaf morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
  
frond_mod <- lm(frond_length_cm ~ niche2, data=traits)
#model diagnostics
frondqq <- qqPlot(residuals(frond_mod)) ##there are a couple of bad points
# identify(frondqq) #not needed but for reference
plot(frond_mod) #evidence to remove observation 226, 228 and 203

frond_mod2 <- lm(frond_length_cm ~ niche2, data=traits[-c(205, 206,226,228,203),])
  plot(frond_mod2)
  qqPlot(frond_mod2)

  Ltest_frond <- leveneTest(frond_length_cm ~ niche2 , data = traits)
  summary(Ltest_frond) # not signficant so variance are equal

bwplot(frond_length_cm ~ niche2 | site , data = traits) 
visreg(frond_mod2)  

summary(frond_mod2)
anova(frond_mod2)

tukey_frond <- glht(frond_mod2, linfct = mcp(niche2 = "Tukey"))
frond_siglets <-cld(tukey_frond)
frond_siglets2 <- frond_siglets$mcletters$Letters
#terrestrial longer frond length than  epi, hemi same as epi


##linear mixed model with species as random (account for species variation)
library(lme4)
library(MuMIn)
library(arm)

boxplot(frond_length_cm ~ niche2, data=traits)

frond_mod4 <- lmer(frond_length_cm ~ niche2 * site + (1|species), 
                   data=traits[-c(205, 206,226,228,203),])
  
  plot(frond_mod4)
  qqnorm(resid(frond_mod4))
  qqline(resid(frond_mod4))

  Anova(frond_mod4) 
  summary(frond_mod4)

  r.squaredGLMM(frond_mod4)
# R2m       R2c 
# 0.256307 0.7870744
#P<o.ooo1
  
# niche effect still present depsite large amount of species variation
  
tukey_frond_mm <- glht(frond_mod4, linfct = mcp(niche2 = "Tukey"))
  frond_siglets_mm <-cld(tukey_frond_mm)
  frond_siglets2_mm <- frond_siglets_mm$mcletters$Letters
  frond_siglets2_mm ##same as before