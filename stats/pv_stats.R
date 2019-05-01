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


boxplot(waterpot_tlp ~ niche2, data=pv) #looks good
hist(pv$waterpot_tlp) #looks good

tlp_mod <- lmer(waterpot_tlp ~ niche2 * site + (1|species), data=pv)
tlp_mod2 <- lmer(waterpot_tlp ~ niche2 + site + (1|species), data=pv)


plot(tlp_mod) #not bad
qqPlot(residuals(tlp_mod)) #not bad

#model summary
Anova(tlp_mod, type="3") #niche effect
anova(tlp_mod, tlp_mod2) #not different
AIC(tlp_mod, tlp_mod2) #use model without interaction

summary(tlp_mod2)
Anova(tlp_mod2, type="3")
r.squaredGLMM(tlp_mod2)
#R2m       R2c
#0.1000985 0.5529221
# niche2    0.04197

visreg(tlp_mod2)
##epiphyte less drought toleratnt 

tukey_tlp <- glht(tlp_mod2, linfct = mcp(niche2 = "Tukey"))
tlp_siglets <-cld(tukey_tlp)

# terrestrial hemi-epiphyte      epiphyte 
# "a"           "a"           "a" 


library(emmeans)
library(multcompView)


test <- emmeans(tlp_mod2, "niche2")
test2 <- CLD(test)
