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
alldata$nitro_area <- with(alldata,lma * (n_perc/100))

#bivariate mixed model

#nitro
lmanitro_mod <- lmer(sqrt(nitro_area) ~ lma  * niche2  + (1|species),data=alldata)
  qqPlot(residuals(lmanitro_mod))#pretty good
  plot(lmanitro_mod) #pretty good

#chl
chlnitro_mod <- lmer(chl_mg_m2 ~ nitro_area  * niche2  + (1|species),data=alldata)
chlnitro_mod2 <- lmer(chl_mg_m2 ~ nitro_area  + niche2  + (1|species),data=alldata)

qqPlot(residuals(chlnitro_mod2))#pretty good
plot(chlnitro_mod2) #skewed


#model summary nitro -----------
Anova(lmanitro_mod, type="3") #interactions
summary(lmanitro_mod)
r.squaredGLMM(lmanitro_mod)
# R2m       R2c
# 0.4811851 0.8606318

Anova(lmanitro_mod, type=3)
visreg(lmanitro_mod, "nitro_area", by="niche2")

library(emmeans)
pairwise <- emmeans(lmanitro_mod, ~ lma  * niche2 )
pairs(pairwise)

#slopes and elevations
nitrolma2 <- sma(nitro_area ~ lma * niche2,data=alldata, 
                    multcomp = TRUE, multcompmethod='adjusted')
summary(nitrolma2) #slopes are equal
#P-value : 0.26588  

nitrolma3 <- sma(nitro_area ~ lma + niche2,
                    data=alldata, multcomp = TRUE,
                    multcompmethod='adjusted') 
summary(nitrolma3) #elevations not equal P-value : 1.1368e-05
#elevations different for terr and epi



#model summary chl -------------
Anova(chlnitro_mod, type="3") #no interactions
anova(chlnitro_mod, chlnitro_mod2) #marginally different
AIC(chlnitro_mod, chlnitro_mod2) #could use without interactions

Anova(chlnitro_mod, type="3") #nothing
summary(chlnitro_mod)
r.squaredGLMM(chlnitro_mod)
