pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "terrestrial", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)
  
  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                    levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)
library(lme4)
library(MuMIn)
library(arm)

#tlp models -----------------------

boxplot(waterpot_tlp ~ niche2, data=pv) #looks good
hist(pv$waterpot_tlp) #looks good

tlp_mod <- lmer(waterpot_tlp ~ niche2 * site + (1|species), data=pv)
tlp_mod2 <- lmer(waterpot_tlp ~ niche2 + site + (1|species), data=pv)

plot(tlp_mod2) #not bad
qqPlot(residuals(tlp_mod2)) #not bad

#model summary
Anova(tlp_mod, type="3") #no effects
anova(tlp_mod, tlp_mod2) #not different
AIC(tlp_mod, tlp_mod2) #use model without interaction

Anova(tlp_mod2, type="3")
r.squaredGLMM(tlp_mod2)
#R2m       R2c
#0.09453121 0.5527911
# niche2    0.04197

tukey_tlp <- glht(tlp_mod2, linfct = mcp(niche2 = "Tukey"))
tlp_siglets <-cld(tukey_tlp)

# terrestrial hemi-epiphyte      epiphyte 
# "a"           "a"           "a" 


library(emmeans)
library(multcompView)

test <- emmeans(tlp_mod2, "niche2")
test2 <- CLD(test)
multcompBoxplot(waterpot_tlp ~ niche2, data=pv)


## osmotic potential --------

boxplot(osmotic_potential ~ niche2, data=pv) #looks good
hist(pv$osmotic_potential) #looks good

op_mod <- lmer(osmotic_potential ~ niche2 * site + (1|species), data=pv)
op_mod2 <- lmer(osmotic_potential ~ niche2 + site + (1|species), data=pv)

plot(tlp_mod2) #not bad
qqPlot(residuals(tlp_mod2)) #not bad

#model summary
Anova(op_mod, type="3") #no effects
anova(op_mod, op_mod2) #not different
AIC(op_mod, op_mod2) #use model without interaction

summary(op_mod2)
Anova(op_mod2, type="3")
r.squaredGLMM(op_mod2)

# R2m       R2c
# [1,] 0.1419392 0.6108792
# 
# niche2       0.009819 **

tukey_op <- glht(op_mod2, linfct = mcp(niche2 = "Tukey"))
op_siglets <-cld(tukey_op)

# terrestrial hemi-epiphyte      epiphyte 
# "a"           "ab"           "b" 

### elasticity -------

boxplot(elasticity ~ niche2, data=pv) #looks good
hist(sqrt(pv$elasticity)) #looks good

e_mod <- lmer(sqrt(elasticity) ~ niche2 * site + (1|species), data=pv)
e_mod2 <- lmer(sqrt(elasticity) ~ niche2 + site + (1|species), data=pv)

plot(e_mod) #not bad
qqPlot(residuals(e_mod)) #not bad

#model summary
Anova(e_mod, type="3") #no effects
anova(e_mod, e_mod2) #not different
AIC(e_mod, e_mod2) #use model with interaction

summary(e_mod)
Anova(e_mod, type="3")
r.squaredGLMM(e_mod)

### capacitance -------

boxplot(capacitance_full ~ niche2, data=pv)
boxplot(capacitance_zero. ~ niche2, data=pv)
boxplot(capacitance_absolute ~ niche2, data=pv)

hist(pv$capacitance_full) 
hist(pv$capacitance_zero.)
hist(pv$capacitance_absolute)

full <- pv[pv$capacitance_full < .2,]
zero <- pv[pv$capacitance_zero. < 1,]
abs <-  pv[pv$capacitance_absolute < .4,]
            
boxplot(capacitance_full ~ niche2, data=full)
boxplot(capacitance_zero. ~ niche2, data=zero)
boxplot(capacitance_absolute ~ niche2, data=abs)

hist(sqrt(full$capacitance_full) )
hist(sqrt(zero$capacitance_zero.))
hist(abs$capacitance_absolute)    

#capacitance full
full_mod <- lmer(sqrt(capacitance_full) ~ niche2 * site + (1|species), data=full)
full_mod2 <- lmer(sqrt(capacitance_full) ~ niche2 + site + (1|species), data=full)

plot(full_mod) #not bad
qqPlot(residuals(full_mod)) #not bad

#model summary
Anova(full_mod2, type="3") #no effects

# mean(full[full$niche2 == "terrestrial", "capacitance_full"]) #0.06
# mean(full[full$niche2 == "epiphyte", "capacitance_full"]) #0.05

#capcitance absolute
abs_mod <- lmer(log10(capacitance_absolute) ~ niche2 * site + (1|species), data=abs)
abs_mod2 <- lmer(log10(capacitance_absolute) ~ niche2 + site + (1|species), data=abs)

plot(abs_mod) #not bad
qqPlot(residuals(abs_mod)) #not bad

#model summary
Anova(abs_mod2, type="3") #no effect


#capcitance zero
zero_mod <- lmer(sqrt(capacitance_zero.) ~ niche2 * site + (1|species), data=zero)
zero_mod2 <- lmer(sqrt(capacitance_zero.) ~ niche2 + site + (1|species), data=zero)

plot(zero_mod) #not bad
qqPlot(residuals(zero_mod)) #not bad

#model summary
Anova(zero_mod, type="3") #niche2

anova(zero_mod, zero_mod2) #not different
AIC(zero_mod, zero_mod2) 

summary(zero_mod)
Anova(zero_mod, type="3")
r.squaredGLMM(zero_mod)

# R2m       R2c
# [1,] 0.280573 0.619161

# niche2    0.009101 *

tukey_zero <- glht(zero_mod, linfct = mcp(niche2 = "Tukey"))
zero_siglets <-cld(tukey_zero)

# terrestrial hemi-epiphyte      epiphyte 
# "b"          "ab"           "a" 

#capcitance zero is lower in epiphytes

terr <- mean(zero[zero$niche2 == "terrestrial", "capacitance_zero."]) #0.34
epi <- mean(zero[zero$niche2 == "epiphyte", "capacitance_zero."]) #0.15

### RWC -------

boxplot(rwc_tlp ~ niche2, data=pv)
hist(sqrt(pv$rwc_tlp)) #looks good

rwc <- pv[pv$rwc_tlp>90,]

boxplot(rwc_tlp ~ niche2, data=rwc)
hist(sqrt(rwc$rwc_tlp))
hist(rwc$rwc_tlp)

rwc_mod <- lmer(rwc_tlp ~ niche2 * site + (1|species), data=rwc)
rwc_mod2 <- lmer(rwc_tlp ~ niche2 + site + (1|species), data=rwc)

plot(rwc_mod) #not bad
qqPlot(residuals(rwc_mod)) #not bad

#model summary
Anova(rwc_mod, type="3") #no effects
anova(rwc_mod, rwc_mod2) #not different
AIC(rwc_mod, rwc_mod2) #use model without interaction

summary(rwc_mod)
Anova(rwc_mod, type="3")
r.squaredGLMM(rwc_mod)

### SWC -------

boxplot(SWC~ niche2, data=pv)
hist(sqrt(pv$SWC)) #looks good

swc <- pv[pv$SWC<7.8,]
boxplot(SWC~ niche2, data=swc)
hist(sqrt(swc$SWC))

swc_mod <- lmer(sqrt(swc$SWC) ~ niche2 * site + (1|species), data=swc)
swc_mod2 <- lmer(sqrt(swc$SWC) ~ niche2 + site + (1|species), data=swc)

plot(swc_mod) #not bad
qqPlot(residuals(swc_mod)) #not bad

#model summary
Anova(swc_mod, type="3") #no effects
anova(swc_mod, swc_mod2) #not different
AIC(swc_mod, swc_mod2) #use model without interaction

summary(swc_mod2)
Anova(swc_mod2, type="3")
r.squaredGLMM(swc_mod2)

#SWC not different between niches