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
Anova(nitro_mod, type="3") #niche but no interaction
anova(nitro_mod, nitro_mod2) #not different
AIC(nitro_mod, nitro_mod2) #model 2 is slighly better

#using model without interaction
summary(nitro_mod2)
Anova(nitro_mod, type="3")
r.squaredGLMM(nitro_mod)
#R2m       R2c
#0.1988045 0.8050532

#niche2        4.8309  2    0.08933

visreg(nitro_mod2)
##slightly lower nitrogen for epiphyes

tukey_nitro <- glht(nitro_mod2, linfct = mcp(niche2 = "Tukey"))
nitro_siglets <-cld(tukey_nitro)

#terrestrial hemi-epiphyte      epiphyte 
#"b"          "ab"           "a" 

#epi lower, hemi is intermediate


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
#R2m       R2c
#0.269034 0.685494

#niche2        17.1304  2  0.0001906 ***
#site           4.7686  1  0.0289836 * 

visreg(c13_mod2)
##d13 higher in epi
##d13 higher in las cruces

tukey_c13_niche <- glht(c13_mod2, linfct = mcp(niche2 = "Tukey"))
c13niche_siglets <-cld(tukey_c13_niche)
#terrestrial hemi-epiphyte      epiphyte 
#"a"           "a"           "b" 


tukey_c13_site <- glht(c13_mod2, linfct = mcp(site = "Tukey"))
c13site_siglets <-cld(tukey_c13_site)
#la_selva las_cruces 
#"a"        "b" 




# 15n ------  #likely ignore these for now

boxplot(d15N ~ niche2, data=leafchem) #maybe on low value, no big deal

d15N_mod <- lmer(d15N ~ niche2 * site + (1|species), data=leafchem)
d15N_mod2 <- lmer(d15N ~ niche2 + site + (1|species), data=leafchem)

hist(leafchem$d15N) #looks good
plot(d15N_mod) #looks good
qqPlot(residuals(d15N_mod)) #looks good

#model summary
Anova(d15N_mod, type="3") #niche & site but no interaction
anova(d15N_mod, d15N_mod2) #not different
AIC(d15N_mod, d15N_mod2) #model one is slighly better

#using model with interaction tem
summary(d15N_mod)
Anova(d15N_mod, type="3") #site effect and marginal niche
r.squaredGLMM(d15N_mod)

#R2m       R2c
#[1,] 0.3833842 0.6705619

#niche2       2.8613  2    0.23915    
#site        29.5920  1  5.332e-08 ***
#niche2:site  4.9591  2    0.08378 .  


visreg(d15N_mod, "niche2", by="site")
##dn15 higher in la selva
##pretty mess interaction, likely terrestrial and epi lower at las cruces only


tukey_n15_site <- glht(d15N_mod, linfct = mcp(site = "Tukey"))
n15site_siglets <-cld(tukey_n15_site)
#la_selva las_cruces 
#"b"        "a" 
