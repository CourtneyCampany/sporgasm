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

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#bivariate mixed model
sizedens <- lmer(log10(sd_mm2) ~ log10(stomatal_size)  * niche2 
                 + (1|species),  data=alldata)

sizedens2 <- lm(sd_mm2 ~ stomatal_size  * niche2 ,  data=alldata)
Anova(sizedens2, type=3)
summary(sizedens2) #r2=.41, p < .00001

anova(lm(sd_mm2 ~ stomatal_size ,  data=terr))
anova(lm(sd_mm2 ~ stomatal_size ,  data=hemi))
anova(lm(sd_mm2 ~ stomatal_size ,  data=epi))

#model diagnostics
par(mar=c(4,4,1,1))
qqPlot(residuals(sizedens2)) 
plot(sizedens2)


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

