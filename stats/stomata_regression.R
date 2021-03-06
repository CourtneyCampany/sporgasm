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

#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

##separate habitat dataframes for all traits -----
terr <- stomata_noout[stomata_noout$niche2 == "terrestrial",]
hemi <- stomata_noout[stomata_noout$niche2 == "hemi-epiphyte" ,]
epi <- stomata_noout[stomata_noout$niche2 == "epiphyte",]

plot(stomatal_size ~ sd_mm2, data=stomata_noout)

#bivariate mixed model
sizedens <- lmer(sqrt(stomatal_size) ~ sqrt(sd_mm2)  * niche2 
                 + (1|species),  data=stomata_noout)

sizedens2 <- lmer(log10(sd_mm2) ~ log10(stomatal_size)  * niche2 
                 + (1|species),  data=stomata_noout)

sizedens3 <- lmer(sqrt(sd_mm2) ~ sqrt(stomatal_size)  + niche2 
                 + (1|species),  data=stomata_noout)

sizedens4 <- lmer(stomatal_size ~ sd_mm2  * niche2 
                 + (1|species),  data=stomata_noout)

#model diagnostics (sqrt trans works best)
qqPlot(residuals(sizedens)) 
plot(sizedens)

Anova(sizedens, type=3) 
anova(sizedens, sizedens3)
AIC(sizedens, sizedens3) #no interaction, choose model without

Anova(sizedens3, type=3) #p = .09

summary(sizedens3)
r.squaredGLMM(sizedens3)
#R2m       R2c
#0.2027541 0.8960833

anova(lm(sd_mm2 ~ stomatal_size ,  data=terr))
anova(lm(sd_mm2 ~ stomatal_size ,  data=hemi))
anova(lm(sd_mm2 ~ stomatal_size ,  data=epi))

#test slopes and elevations
sizedens3 <- sma(sd_mm2 ~ stomatal_size * niche2,
                    data=alldata, multcomp = TRUE,
                    multcompmethod='adjusted') #slopes not equal
summary(sizedens3) #slopes not equal, all different, all relationships sig

sizedens4 <- sma(sd_mm2 ~ stomatal_size + niche2,
                    data=alldata, multcomp = TRUE,
                    multcompmethod='adjusted') #slopes not equal
summary(sizedens4) #elevations not equal
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


##For reviewer1


selva <- stomata_noout[stomata_noout$site == "la_selva",]
cruces <- stomata_noout[stomata_noout$site == "las_cruces" ,]

selva_mod <- lmer(sqrt(sd_mm2) ~ niche2  + (1|species), data=selva)
cruces_mod <- lmer(sqrt(sd_mm2) ~ niche2 + (1|species), data=cruces)

#laselva
plot(selva_mod)
qqPlot(residuals(selva_mod))
summary(selva_mod)
Anova(selva_mod, type="3") #only niche effect
r.squaredGLM(selva_mod)
tukey_selva <- glht(selva_mod, linfct = mcp(niche2 = "Tukey"))
cld(tukey_selva) #same

#lascruces
plot(cruces_mod)
qqPlot(residuals(cruces_mod))
summary(cruces_mod)
Anova(cruces_mod, type="3") #only niche effect
r.squaredGLM(cruces_mod)
tukey_cruces <- glht(cruces_mod, linfct = mcp(niche2 = "Tukey"))
cld(tukey_cruces) #not same

