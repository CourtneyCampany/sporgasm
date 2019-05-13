source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

#chemistry data
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "terrestrial", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)
  
#sla data
sla <- read.csv("calculated_data/fern_sla.csv")

##merge lma and nitrogen
nitro <- merge(leafchem, sla, all=TRUE)  
nitro$lma_g_m2 <- with(nitro, 1/(sla_cm2g/10000))
nitro$nitro_area <- with(nitro,lma_g_m2 * (n_perc/100))

#reorder from ground to canopy 
nitro$niche2<-factor(nitro$niche2, 
              levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##is nitro and sla corrdinated?
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(lme4)
library(MuMIn)

nitro_mod <- lmer(n_perc ~ lma_g_m2 * niche2 + site + (1|species), 
                   data=nitro[nitro$lma_g_m2 < 600,])

# nitro_mod2 <- lmer(n_perc ~ lma_g_m2 * niche2 * site + (1|species), 
#                   data=nitro2[nitro2$lma_g_m2 < 600,])


# plot(nitro_mod) #ok
# qqPlot(residuals(nitro_mod)) #ok

#model summary (must use model with interaction but do I need site included?)
Anova(nitro_mod, type="3") 
# Anova(nitro_mod2, type="3") 
# anova(nitro_mod, nitro_mod2) #different
# AIC(nitro_mod, nitro_mod2) #first model is  better (no site interaction)

#use model  interaction
# visreg(nitro_mod, "lma_g_m2", by="niche2")
r.squaredGLMM(nitro_mod)
# R2m       R2c
# [1,] 0.250495 0.8111689

summary(nitro_mod)
library(emmeans)
emmip(nitro_mod, lma_g_m2 ~ niche2)
emmeans(nitro_mod, pairwise ~ lma_g_m2 : niche2)
library(phia)
testInteractions(nitro_mod, pairwise="niche2", slope="lma_g_m2") ##slopes are different
testInteractions(nitro_mod, custom=list(niche2=c(1,0,0)),
                 slope="lma_g_m2", adjustment="none")
#terr = -0.0045704  P=0.005226
testInteractions(nitro_mod, custom=list(niche2=c(0,1,0)),
                 slope="lma_g_m2", adjustment="none")
#hemi = -0.010101  P < 0.0001
testInteractions(nitro_mod, custom=list(niche2=c(0,0,1)),
                 slope="lma_g_m2", adjustment="none")
#epi = 0.00022938  P=0.8127

#significantly different from zero except for epi (all negative )

