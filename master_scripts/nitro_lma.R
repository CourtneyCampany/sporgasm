source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

#chemistry data
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "hemi-epiphyte", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)
  
  #reorder from ground to canopy 
  leafchem$niche2<-factor(leafchem$niche2, 
                          levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#sla data

sla <- read.csv("calculated_data/fern_sla.csv")
  #reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))



nitro <- merge(leafchem, sla, all=TRUE)  
nitro$lma_g_m2 <- with(nitro, 1/(sla_cm2g/10000))
nitro$nitro_area <- with(nitro,lma_g_m2 * (n_perc/100))

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

# summary(nitro_mod)
# library(emmeans)
# emmip(nitro_mod, lma_g_m2 ~ niche2)
# emmeans(nitro_mod, pairwise ~ lma_g_m2 : niche2)
# library(phia)
# testInteractions(nitro_mod, pairwise="niche2", slope="lma_g_m2") ##slopes are different
# testInteractions(nitro_mod, custom=list(niche2=c(1,0,0)), 
#                  slope="lma_g_m2", adjustment="none")
# #terr = -0.0045704  P=0.005226 
# testInteractions(nitro_mod, custom=list(niche2=c(0,1,0)), 
#                  slope="lma_g_m2", adjustment="none")
# #hemi = -0.010101  P < 0.0001 
# testInteractions(nitro_mod, custom=list(niche2=c(0,0,1)), 
#                  slope="lma_g_m2", adjustment="none")
# #epi = 0.00022938  P=0.8127

#significantly different from zero except for epi (all negative )


###simple modesl for visreg:
terr <- droplevels(nitro[nitro$niche2 == "terrestrial", ])
hemi <- droplevels(nitro[nitro$niche2 == "hemi-epiphyte", ])

terr_mod <- lm(n_perc ~ lma_g_m2, data=terr[terr$lma_g_m2 < 600,])

hemi_mod <- lm(n_perc ~ lma_g_m2, data=hemi[hemi$lma_g_m2 < 600,])



#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .7), alpha(trtcols[2], .7),alpha(trtcols[3], .7))

library(plotrix)


# jpeg(filename = "output/nitro_lma.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mar=c(5,5,1,1))
plot(n_perc ~ lma_g_m2, data=nitro[nitro$lma_g_m2 < 600,], ylim=c(0,6), xlim=c(0,525),
     ylab="Foliar Nitrogen (%)",xlab=lmalab, type='n')
     ablineclip(terr_mod, x1 = 30, x2 = 535, lty=2, lwd=2, col="red")
     predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
     predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
     points(n_perc ~ lma_g_m2, data=nitro[nitro$lma_g_m2 < 600,], 
            col= trtcols2[niche2], pch=16, cex=1.5)
     legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)
     text(455,.2,"LMA x Niche, P < 0.001")

# dev.off()  

#lma x niche, p <0.001