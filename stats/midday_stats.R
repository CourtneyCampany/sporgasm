# water potential stats

##run stats------
library(visreg)
library(multcomp)
library(car)
library(lme4)
library(MuMIn)


wp <- read.csv("calculated_data/waterpotential_middday.csv")

boxplot((water_potential *-1) ~ niche2, data=wp)  
boxplot(water_potential ~ site, data=wp)  
hist(wp$water_potential) #transform

wp_mod <- lmer(log10(water_potential) ~ niche2 * site+ (1|species), data=wp)
wp_mod2 <- lmer(log10(water_potential) ~ niche2 + site + (1|species), data=wp)

plot(wp_mod)
qqPlot(residuals(wp_mod))

#model summary
Anova(wp_mod, type="3") #site but no interaction
anova(wp_mod, wp_mod2) #not different
AIC(wp_mod, wp_mod2) #useorginal

#use model without interaction
# summary(stipe_mod8)
Anova(stipe_mod8, type="3")
r.squaredGLMM(stipe_mod8)