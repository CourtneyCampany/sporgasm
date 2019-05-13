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


###simple modesl for predline
terr <- droplevels(nitro[nitro$niche2 == "terrestrial", ])
hemi <- droplevels(nitro[nitro$niche2 == "hemi-epiphyte", ])
epi <- droplevels(nitro[nitro$niche2 == "epiphyte", ])

terr_mod <- lm(n_perc ~ lma_g_m2, data=terr[terr$lma_g_m2 < 600,])

hemi_mod <- lm(n_perc ~ lma_g_m2, data=hemi[hemi$lma_g_m2 < 600,])

epi_mod <- lm(n_perc ~ lma_g_m2, data=epi[epi$lma_g_m2 < 600,])

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

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(n_perc ~ lma_g_m2, data=nitro[nitro$lma_g_m2 < 600,], ylim=c(0,6), xlim=c(0,525),
     ylab="Foliar Nitrogen (%)",xlab=lmalab, type='n')
     points(n_perc ~ lma_g_m2, data=nitro[nitro$lma_g_m2 < 600,], 
            col= trtcols2[niche2], pch=16, cex=1.25)
     predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
     predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
     predline(epi_mod, col=trtcols[3], lwd=2, lty=2)

     legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)
     text(455,.2,"LMA x Niche, P < 0.001")

# dev.off()  

#lma x niche, p <0.001