library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

#regression between stipe length and xylem area

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata_nona <- alldata[complete.cases(alldata$xylem_area_um2),]
alldata2 <- alldata_nona[alldata_nona$xylem_area_mm2 < .8,]
alldata3 <- alldata2[complete.cases(alldata2$xylem_area_mm2) & 
                       complete.cases(alldata2$species),]
#outliers present with hemi
# boxplot(xylem_area_mm2 ~ niche2, data=alldata)
alldata4 <- alldata3[-c(126,127,129),]

#bivariate mixed model
xylemlength <- lmer(sqrt(stipe_length_cm) ~ sqrt(xylem_area_mm2)  * niche2  + (1|species),
                   data=alldata4)

#model diagnostics
qqPlot(residuals(xylemlength)) 
plot(xylemlength)

#model summary
Anova(xylemlength, type="3") #no interactions
r.squaredGLMM(xylemlength)
# R2m       R2c
# [1,] 0.3313526 0.8810719

xylemlength2 <- sma(sqrt(stipe_length_cm) ~ sqrt(xylem_area_mm2) * niche2,
                    data=alldata4, multcomp = TRUE,
                    multcompmethod='adjusted') #slopes not equal
summary(xylemlength2) #slopes not equal, all different, all relationships sig

xylemlength3 <- sma(sqrt(stipe_length_cm) ~ sqrt(xylem_area_mm2)+ niche2,
                    data=alldata4, multcomp = TRUE,
                    multcompmethod='adjusted') #slopes not equal
summary(xylemlength3) #elevations not equal
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

