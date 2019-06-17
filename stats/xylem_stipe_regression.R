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
alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]
alldata3 <- alldata2[complete.cases(alldata2$xylem_area_mm2) & 
                       complete.cases(alldata2$species),]
alldata3$stipe_nozero <- alldata3$stipe_length_cm + .1


#bivariate mixed model
xylemlength <- lmer(log10(xylem_area_mm2) ~ log10(stipe_nozero) * niche2  + (1|species),
                   data=alldata3)

#model diagnostics
qqPlot(residuals(xylemlength)) 
plot(xylemlength)

#model summary
Anova(xylemlength, type="3") #no interactions
r.squaredGLMM(xylemlength)


xylemlength3 <- sma(xylem_area_mm2 ~ stipe_nozero * niche2,
                    data=alldata3, multcomp = TRUE,log="xy",
                    multcompmethod='adjusted') #slopes not equal
summary(xylemlength3) #slopes not equal, all different

xylemlength4 <- sma(xylem_area_mm2 ~ stipe_nozero + niche2,
                    data=alldata3, multcomp = TRUE,log="xy",
                    multcompmethod='adjusted') #slopes not equal
summary(xylemlength4) #elevations different


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

