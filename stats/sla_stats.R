
sla <- read.csv("calculated_data/fern_sla.csv")
  

#reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

Ltest_frond <- leveneTest(sla_cm2g ~ niche2 , data = sla)


##full mixed model---------------------
boxplot(sla_cm2g ~ niche2, data=sla)

sla_mod2 <- lmer(sqrt(sla_cm2g) ~ niche2 * site + (1|species), data=sla)
sla_mod3 <- lmer(sqrt(sla_cm2g) ~ niche2 + site + (1|species), data=sla)

hist(sla$sla_cm2g)
skewness(sla_mod2$residuals) #less than 1
plot(sla_mod2)
qqPlot(residuals(sla_mod2))

#model summary
Anova(sla_mod2, type="3") #niche but no interaction
anova(sla_mod2, sla_mod3) #not different
AIC(sla_mod2, sla_mod3) #model with interaction in better

#use model with interaction
summary(sla_mod3)
Anova(sla_mod2, type="3")
r.squaredGLMM(sla_mod3)
#R2m       R2c
#0.134389 0.8231503

#niche2        7.2506  2    0.02664

visreg(sla_mod3)
##slightly higher SD at las cruces

tukey_sla <- glht(sla_mod3, linfct = mcp(niche2 = "Tukey"))
sla_siglets <-cld(tukey_sla)

#terrestrial hemi-epiphyte      epiphyte 
# "b"           "ab"           "a"

terr <- mean(sla[sla$niche2 == "terrestrial", "sla_cm2g"]) #88.6
epi <- mean(sla[sla$niche2 == "epiphyte", "sla_cm2g"]) #57.4





