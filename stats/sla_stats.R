
sla <- read.csv("calculated_data/fern_sla.csv")

##too much variance within peculma pectinata (something is wrong)
##toom much variance within oleandra articulata

##double check reliability of asplenium_uniseriale (really high)
##pictures show it is really thin though


##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

#simple model
sla_mod <- lm(sla_cm2g ~ niche2, data=sla)

#model diagnostics
qqPlot(sla_mod) 
plot(sla_mod) 

#asplenium uniseriale is a problem, get rid for now but double check data

sla2 <- droplevels(sla[sla$species != "asplenium_uniseriale",])

sla_mod2 <- lm(1/sla_cm2g ~ niche2, data=sla2)

#model diagnostics
qqPlot(sla_mod2) 
plot(sla_mod2) 


skewness(sla_mod2$residuals) #less than 1
kurtosis(sla_mod2$residuals) 
hist(sla_mod2$residuals)

summary(sla_mod2)
anova(sla_mod2) #broad differences in niche

Ltest_frond <- leveneTest(1/sla_cm2g ~ niche2 , data = sla)
Ltest_frond ##variances not equal!!
