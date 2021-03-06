source("functions_packages/basic_functions.R")

## stats on leaf chemistry-------
library(visreg)
library(lme4)
library(MuMIn)
library(multcomp)
library(car)
library(lattice)
library(outliers)


leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "terrestrial", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)
  leafchem$percN <- leafchem$n_perc/100

  #reorder from ground to canopy 
  leafchem$niche2<-factor(leafchem$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


## run each model with interaction of site and niche and see if required


##nitro ------

nitro_mod <- lmer(asin(sqrt(percN)) ~ niche2 * site + (1|species), data=leafchem)
nitro_mod2 <- lmer(asin(sqrt(percN)) ~ niche2 + site + (1|species), data=leafchem)

boxplot(n_perc ~ niche2, data=leafchem) #no outlier present
hist(leafchem$n_perc) #looks good
plot(nitro_mod) #looks good
qqPlot(residuals(nitro_mod)) #looks good

#model summary
anova(nitro_mod, nitro_mod2) #not different
AIC(nitro_mod, nitro_mod2) 

#using model without interaction
summary(nitro_mod2)
Anova(nitro_mod2, type="3")
r.squaredGLMM(nitro_mod2)
#R2m       R2c
#0.1740686 0.7982922

#niche2   0.007562

tukey_nitro <- glht(nitro_mod2, linfct = mcp(niche2 = "Tukey"))
nitro_siglets <-cld(tukey_nitro)

#terrestrial hemi-epiphyte      epiphyte 
#"b"          "ab"           "a" 

#epi lower than other croups
terr_N <- mean(leafchem[!leafchem$niche2 == "epiphyte", "percN"],na.rm=TRUE) #.0295
epi_N <- mean(leafchem[leafchem$niche2 == "epiphyte", "percN"], na.rm=TRUE) #.02079



# c13 ---------
boxplot(d13C ~ niche2, data=leafchem) #couple of terrestrial outliers

#remove outlier and see if changes results
c13dat <- leafchem[!leafchem$genusspecies == "bleschi",]
##changes the results for hemi, I kept used new dataset for now

c13_mod <- lmer(d13C ~ niche2 * site + (1|species), data=c13dat)
  # relgrad <- with(nitro_mod@optinfo$derivs,solve(Hessian,gradient))
  # max(abs(relgrad)) #failed covergence is fine when this is <.001
c13_mod2 <- lmer(d13C ~ niche2 + site + (1|species), data=c13dat)


hist(leafchem$d13C) #looks good
plot(c13_mod) #looks good
qqPlot(residuals(c13_mod)) #looks good

#model summary
Anova(c13_mod, type="3") #niche & site but no interaction
anova(c13_mod, c13_mod2) #not different
AIC(c13_mod, c13_mod2) #close together, use simple model

#using model without interaction
summary(c13_mod2)
Anova(c13_mod2, type="3")
r.squaredGLMM(c13_mod2)
#R2m       R2
#0.2331338 0.73046

# niche2        10.4991  2   0.005250 ** 
#   site           6.6734  1   0.009786 ** 

visreg(c13_mod2)
##d13 higher in epi
##d13 higher in las cruces

tukey_c13_niche <- glht(c13_mod2, linfct = mcp(niche2 = "Tukey"))
c13niche_siglets <-cld(tukey_c13_niche)
#terrestrial hemi-epiphyte      epiphyte 
#"a"           "ab"           "b" 


epi <- mean(c13dat[c13dat$niche2 == "epiphyte", "d13C"],na.rm=TRUE) #-32.46
terri <- mean(c13dat[c13dat$niche2 == "terrestrial", "d13C"], na.rm=TRUE) #-34.07

epi_iso_se <- se(c13dat[c13dat$niche2 == "epiphyte", "d13C"]) #0.20
noepi_iso_se <- se(c13dat[!c13dat$niche2 == "epiphyte", "d13C"]) #0.17

tukey_c13_site <- glht(c13_mod2, linfct = mcp(site = "Tukey"))
c13site_siglets <-cld(tukey_c13_site)
#la_selva las_cruces 
#"a"        "b" 

ls <- mean(c13dat[c13dat$site == "la_selva", "d13C"],na.rm=TRUE) #-33.98324
lc <- mean(c13dat[c13dat$site == "las_cruces", "d13C"], na.rm=TRUE) #-32.95949

ls_se <- se(c13dat[c13dat$site == "la_selva", "d13C"]) #0.15
lc_se <- se(c13dat[c13dat$site == "las_cruces", "d13C"]) #0.26

boxplot(d13C ~ site, data=c13dat) 

# 15n ------  #likely ignore these for now

boxplot(d15N ~ niche2, data=leafchem) #maybe on low value, no big deal

d15N_mod <- lmer(d15N ~ niche2 * site + (1|species), data=leafchem)
d15N_mod2 <- lmer(d15N ~ niche2 + site + (1|species), data=leafchem)

hist(leafchem$d15N) #looks good
plot(d15N_mod) #looks good
qqPlot(residuals(d15N_mod)) #looks good

#model summary
Anova(d15N_mod, type="3") #site but no interaction
anova(d15N_mod, d15N_mod2) #not different
AIC(d15N_mod, d15N_mod2) #model one is slighly better

#using model with interaction tem
summary(d15N_mod)
Anova(d15N_mod, type="3") #site effect only
r.squaredGLMM(d15N_mod)

#R2m       R2c
#[1,] 0.3666595 0.6715925

#site        29.5920  1  3.48e-06

visreg(d15N_mod, "niche2", by="site")
##dn15 higher in la selva

tukey_n15_site <- glht(d15N_mod, linfct = mcp(site = "Tukey"))
n15site_siglets <-cld(tukey_n15_site)
#la_selva las_cruces 
#"b"        "a" 
