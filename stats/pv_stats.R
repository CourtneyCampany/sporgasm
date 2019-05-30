pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
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

summary(tlp_mod3)
Anova(tlp_mod2, type="3")
r.squaredGLMM(tlp_mod2)
#R2m       R2c
#0.1000985 0.5529221
# niche2    0.04197

visreg(tlp_mod2)
##epiphyte less drought toleratnt 

tukey_tlp <- glht(tlp_mod3, linfct = mcp(niche2 = "Tukey"))
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

summary(om_mod2)
Anova(om_mod2, type="3")
r.squaredGLMM(om_mod2)

# R2m       R2c
# [1,] 0.144529 0.6102061
# 
# niche2        9.5179  2   0.008575 ** 

visreg(op_mod2)
##less solutes in epiphytes

tukey_op <- glht(op_mod2, linfct = mcp(niche2 = "Tukey"))
op_siglets <-cld(tukey_op)

# terrestrial hemi-epiphyte      epiphyte 
# "a"           "a"           "b" 

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

hist(full$capacitance_full) 
hist(zero$capacitance_zero.)
hist(abs$capacitance_absolute)    

#capcitance full
full_mod <- lmer(log10(capacitance_full) ~ niche2 * site + (1|species), data=full)
full_mod2 <- lmer(log10(capacitance_full) ~ niche2 + site + (1|species), data=full)

plot(full_mod) #not bad
qqPlot(residuals(full_mod)) #not bad

#model summary
Anova(full_mod2, type="3") #no effects

#capcitance absolute
abs_mod <- lmer(log10(capacitance_absolute) ~ niche2 * site + (1|species), data=full)
abs_mod2 <- lmer(log10(capacitance_absolute) ~ niche2 + site + (1|species), data=full)

plot(abs_mod) #not bad
qqPlot(residuals(abs_mod)) #not bad

#model summary
Anova(abs_mod2, type="3") #no effect


#capcitance zero
zero_mod <- lmer(log10(capacitance_zero.) ~ niche2 * site + (1|species), data=full)
zero_mod2 <- lmer(log10(capacitance_zero.) ~ niche2 + site + (1|species), data=full)

plot(zero_mod) #not bad
qqPlot(residuals(zero_mod)) #not bad

#model summary
Anova(zero_mod, type="3") #niche2

anova(zero_mod, zero_mod2) #not different
AIC(zero_mod, zero_mod2) #use model with interaction

summary(zero_mod)
Anova(zero_mod, type="3")
r.squaredGLMM(zero_mod)

# R2m       R2c
# [1,] 0.3180491 0.6268394

# niche2       9.1368  2    0.01037 *
visreg(zero_mod)

tukey_zero <- glht(zero_mod, linfct = mcp(niche2 = "Tukey"))
zero_siglets <-cld(tukey_zero)

# terrestrial hemi-epiphyte      epiphyte 
# "b"          "ab"           "a" 

#capcitance zero is lower in epiphytes

terr <- mean(zero[zero$niche2 == "terrestrial", "capacitance_zero."]) #0.3455018
epi <- mean(zero[zero$niche2 == "epiphyte", "capacitance_zero."]) #0.1583137


