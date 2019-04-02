
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



##full mixed model---------------------
boxplot(sla_cm2g ~ niche2, data=sla2) #lots of outliers

sla_mod2 <- lmer(sla_cm2g ~ niche2 * site + (1|species), data=sla2)
sla_mod2 <- lmer(sla_cm2g ~ niche2 + site + (1|species), data=sla2)


hist(sd_new$sd_mm2)
plot(sd_mod2)
qqPlot(residuals(sd_mod2))

#model summary
Anova(sd_mod2, type="3") #niche but no interaction
anova(sd_mod2, sd_mod3) #not different
AIC(sd_mod2, sd_mod3) #model 2 is slighly better

#use model without interaction
summary(sd_mod3)
Anova(sd_mod3, type="3")
r.squaredGLMM(sd_mod3)
#R2m       R2c
#0.3069365 0.9220326

#niche2       14.3531  2  0.0007643 ***
# site          2.7325  1  0.0983235 .  

visreg(sd_mod3)
##slightly higher SD at las cruces

tukey_sd3 <- glht(sd_mod3, linfct = mcp(niche2 = "Tukey"))
sd3_siglets <-cld(tukey_sd3)

#terrestrial hemi-epiphyte      epiphyte 
# "b"           "a"           "a"

terr_sd <- mean(sd_new[sd_new$niche2 == "terrestrial", "sd_mm2"]) #68.2
notterr_sd <- mean(sd_new[!sd_new$niche2 == "terrestrial", "sd_mm2"]) #37.1





