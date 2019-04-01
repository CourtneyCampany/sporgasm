## stats on basic leaf morphology-------
library(visreg)
library(lme4)
library(MuMIn)
library(multcomp)
library(car)
library(lattice)
library(outliers)

stodens <- read.csv("calculated_data/stomata_density.csv")
  stodens$niche2 <- gsub("climber", "terrestrial", stodens$niche)
  stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + genusspecies,
                          data=stodens, FUN=mean, keep.names = TRUE)

#basic stats
chisq.out.test(sd_agg$sd_mm2) #2448 is an outier
hist(sd_agg$sd_mm2)

#simple model check
sd_mod <- lm(sd_mm2 ~ niche2, data=sd_agg)
#model diagnostics
qqPlot(residuals(sd_mod)) #pretty good
residualPlot(sd_mod)
plot(sd_mod) ##negative skewed so we need a transformation
skewness(sd_mod$residuals) 

visreg(sd_mod)
summary(sd_mod)
anova(sd_mod)

tukey_sd <- glht(sd_mod, linfct = mcp(niche2 = "Tukey"))
sd_siglets <-cld(tukey_sd)

### boxplot shows outliers from epiphyte, can see it on visreg too
### drop oleandra articulata

sd_new <- droplevels(sd_agg[!sd_agg$genusspecies == "oleart",])

##full mixed model:
sd_mod2 <- lmer(sqrt(sd_mm2) ~ niche2 * site + (1|species), data=sd_new)
sd_mod3 <- lmer(sqrt(sd_mm2) ~ niche2 + site + (1|species), data=sd_new)

boxplot(sd_mm2 ~ niche2, data=sd_new) #no outlier present
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




