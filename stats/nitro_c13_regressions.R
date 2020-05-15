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

d13c_nitro_mod <- lmer(d13C ~ nitro_area + niche2 
                     + (1|species),data=alldata)

d13c_nitro_mod2 <- lmer(d13C ~ nitro_area * niche2 
                       + (1|species),data=alldata)
Anova(d13c_nitro_mod2, type=3)
visreg(d13c_nitro_mod2, "nitro_area", by="niche2")

#model summary
Anova(d13c_nitro_mod, type="3") #no interactions
anova(d13c_nitro_mod, d13c_nitro_mod2) #marginally different
AIC(d13c_nitro_mod, d13c_nitro_mod2) #could use without interactions

d13c_nitro_terr <- lmer(d13C ~ nitro_area + (1|species),data=terr)
d13c_nitro_hemi <- lmer(d13C ~ nitro_area + (1|species),data=hemi)
d13c_nitro_epi <- lmer(d13C ~ nitro_area + (1|species),data=epi)
Anova(d13c_nitro_terr)
Anova(d13c_nitro_hemi)
Anova(d13c_nitro_epi)

#model diagnostics
qqPlot(residuals(d13c_nitro_mod))# good
plot(d13c_nitro_mod) # good

#model summary
Anova(d13c_nitro_mod, type="3") #no interactions
#p = 0.0044

visreg(d13c_nitro_mod, "nitro_area", by="niche2", overlay=TRUE)
summary(d13c_nitro_mod)
r.squaredGLMM(d13c_nitro_mod)
# R2m       R2c
#0.1729185 0.7329421

library(emmeans)
pairwise <- emmeans(d13c_nitro_mod, ~ nitro_area  * niche2 )
pairs(pairwise)

#slopes and elevations
d13cnitro2 <- sma(d13C ~ nitro_area * niche2,data=alldata, 
                    multcomp = TRUE, multcompmethod='adjusted')
summary(d13cnitro2) #slopes not equal, 
#P-value : <0.0001, signifacnt terre and epi different slopes
d13nitro3 <- sma(d13C ~ nitro_area + niche2,
                    data=alldata, multcomp = TRUE,
                    multcompmethod='adjusted') 
summary(d13nitro3) #elevations not equal P-value : <0.00001 
#all life forms differ


# ------------------------------------------------------------
#   Results of comparing lines among groups.
# 
# H0 : slopes are equal.
# Likelihood ratio statistic : 35.97 with 2 degrees of freedom
# P-value : 1.5429e-08 
# ------------------------------------------------------------
#   
#   H0 : no difference in elevation.
# Wald statistic: 57.63 with 2 degrees of freedom
# P-value : 3.062e-13 
# --------------------------------------------------------------





