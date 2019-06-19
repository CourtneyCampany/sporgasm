library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <- with(alldata, 1/(sla_cm2g/10000)) #g m-2

#bivariate mixed model
lmanitro_mod <- lmer(n_perc ~ lma  * niche2 
                    + (1|species),data=alldata)

#model diagnostics
windows()
qqPlot(residuals(lmanitro_mod))#pretty good
plot(lmanitro_mod) #pretty good

#model summary
Anova(lmanitro_mod, type="3") #interactions
summary(lmanitro_mod)
r.squaredGLMM(lmanitro_mod)
# R2m       R2c
# [1,] 0.2462649 0.7911061

library(emmeans)
pairwise <- emmeans(lmanitro_mod, ~ lma  * niche2 )
pairs(pairwise)

#slopes and elevations
xylemlength2 <- sma(n_perc ~ lma * niche2,data=alldata, 
                    multcomp = TRUE, multcompmethod='adjusted')
summary(xylemlength2) #slopes not equal, all different, all relationships sig
#P-value : 0.000001 
xylemlength3 <- sma(n_perc ~ lma + niche2,
                    data=alldata, multcomp = TRUE,
                    multcompmethod='adjusted') 
summary(xylemlength3) #elevations not equal P-value : 0.017624 
#elevations different for hemi-epi, but similar for terrstrial epi


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

