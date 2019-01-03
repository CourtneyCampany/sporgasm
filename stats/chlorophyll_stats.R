
traits <- read.csv("calculated_data/fern_traits.csv")

  ## Climber seems to be close to terrestrial and it still is technically-------
  ## create new variable that adds climber to terrestrial category
  traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  ##add interaction term
  traits$nichesite <- interaction(traits$niche2, traits$site) 

chloro <- traits[complete.cases(traits$chl_mg_m2),]  

##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

chl_mod <- lm(chl_mg_m2 ~ niche2, data=chloro)

#model diagnostics
qqPlot(chl_mod) 
plot(chl_mod) #looks pretty good
skewness(chl_mod$residuals) #less than 1
kurtosis(chl_mod$residuals) 
hist(chl_mod$residuals)

summary(chl_mod)
anova(chl_mod) #broad differences in niche

Ltest_frond <- leveneTest(chl_mg_m2 ~ niche2 , data = chloro)
Ltest_frond ##all good

##add site to model

chl_mod2 <- lm(chl_mg_m2 ~ niche2 * site, data=chloro)
summary(chl_mod2)
anova(chl_mod2) ##no effect of site

visreg(chl_mod2)

##since site is non-signifincant, run a mixed model with site as random effect

##linear mixed model with site as random (account for site variation)
library(lme4)
library(MuMIn)
library(arm)

chl_mod3 <- lmer(chl_mg_m2 ~ niche2 + (1|site), data=chloro)
#singular fit, so appears no vairation assocaited with site
plot(chl_mod3)
qqplot(chl_mod3)

Anova(chl_mod3)
summary(chl_mod3)

r.squaredGLMM(chl_mod3) #confirms singlur fit

#or 
tukey_chl <- summary(glht(chl_mod3, linfct=mcp(niche2="Tukey")))  
chl_siglets <-cld(tukey_chl)
chl_siglets2 <- chl_siglets$mcletters$Letters

visreg(chl_mod3) ##epiphte has lower chlorphyll
