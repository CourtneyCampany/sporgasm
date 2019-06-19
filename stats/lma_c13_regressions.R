library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)
library(visreg)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <- with(alldata, 1/(sla_cm2g/10000)) #g m-2
alldata$nitro_area <- with(alldata,lma * (n_perc/100))

terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte",]
epi <- alldata[alldata$niche2 == "epiphyte",]

#bivariate mixed model ------
d13c_lma_mod <- lmer(d13C ~ lma  * niche2 
                    + (1|species),data=alldata)
#only for hemi-epiphyte (tons of species variation)

d13c_nitro_mod <- lmer(d13C ~ n_perc * niche2 
                     + (1|species),data=alldata)
#only for terrestrail

d13c_nitro_mod2 <- lmer(d13C ~ nitro_area * niche2 
                       + (1|species),data=alldata)
Anova(d13c_nitro_mod2, type=3)
visreg(d13c_nitro_mod, "nitro_area", by="niche2")

d13c_nitro_terr <- lmer(d13C ~ nitro_area + (1|species),data=terr)
d13c_nitro_hemi <- lmer(d13C ~ nitro_area + (1|species),data=hemi)
d13c_nitro_epi <- lmer(d13C ~ nitro_area + (1|species),data=epi)
Anova(d13c_nitro_terr)
Anova(d13c_nitro_hemi)
Anova(d13c_nitro_epi)

#model diagnostics
qqPlot(residuals(d13c_nitro_mod2))# good
plot(d13c_nitro_mod2) # good

#model summary
Anova(d13cnitro_mod, type="3") #interactions
#p = 0.0002
windows()
visreg(d13c_lma_mod, "nitro_area", by="niche2", overlay=TRUE)
summary(d13c_lma_mod)
r.squaredGLMM(d13c_nitro_mod2)
# R2m       R2c
# [1,] 0.2266021 0.7305571

library(emmeans)
pairwise <- emmeans(d13c_nitro_mod2, ~ nitro_area  * niche2 )
pairs(pairwise)

#slopes and elevations
d13clma2 <- sma(d13C ~ lma * niche2,data=c13data, 
                    multcomp = TRUE, multcompmethod='adjusted')
summary(d13clma2) #slopes not equal, 
#P-value : 0.000001, signifacnt terre and hemi have similar slopes
d13clma3 <- sma(d13C ~ lma + niche2,
                    data=c13data, multcomp = TRUE,
                    multcompmethod='adjusted') 
summary(d13clma3) #elevations not equal P-value : 0.017624 
#terrestrial and hemi have different elevations


# ------------------------------------------------------------
#   Results of comparing lines among groups.
# 
# H0 : slopes are equal.
# Likelihood ratio statistic : 65.94 with 2 degrees of freedom
# P-value : 4.774e-15 
# ------------------------------------------------------------
#   
#   H0 : no difference in elevation.
# Wald statistic: 9.036 with 2 degrees of freedom
# P-value : 0.010912 
# ------------------------------------------------------------

