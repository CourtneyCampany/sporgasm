source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")

#relationship between tlp and bulk tissue modulus of elasticity

pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)

  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#are tlp and elasticity related?
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(lme4)
library(MuMIn)

pv_mod <- lmer(waterpot_tlp ~ elasticity * niche2 * site + (1|species),data=pv)
pv_mod2 <- lmer(waterpot_tlp ~ elasticity * niche2 + site + (1|species),data=pv)
pv_mod3 <- lmer(waterpot_tlp ~ elasticity + niche2 + site + (1|species), data=pv)
pv_mod_simple <- lm(waterpot_tlp ~ elasticity ,data=pv)

Anova(pv_mod3, type="3") 

anova(pv_mod2, pv_mod3) #no interactions
AIC(pv_mod2, pv_mod3)
##use model with no interactions (simple model)

r.squaredGLMM(pv_mod3)
# R2m       R2c
# 0.1383438 0.5275806

visreg(pv_mod3)

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

#labels
elasticity_lab <- expression(paste(epsilon, "  (MPa)")) 
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))


# jpeg(filename = "output/tlp_E.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mar=c(5,5,1,1))
plot(waterpot_tlp ~ elasticity,data = pv, ylim=c(-2,0),xlim=c(0,65),
ylab=tlp_lab,xlab=elasticity_lab, type='n')
predline(pv_mod_simple, lwd=2, lty=2)
points(waterpot_tlp ~ elasticity,data = pv, col=trtcols2[niche2], pch=16, cex=1.5)
legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)
text(55,-1.95,"P = 0.007")

# dev.off()  
