source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")

#relationship between tlp and bulk tissue modulus of elasticity

pv <- read.csv("calculated_data/pv_curves2.csv")
   pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
   pv$niche2 <- as.factor(pv$niche2)

#reorder from ground to canopy 
pv$niche2<-factor(pv$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

om_mod_simple <- lm(waterpot_tlp ~ osmotic_potential ,data=pv)
elas_mod_simple <- lm(waterpot_tlp ~ elasticity ,data=pv)

##separate habitat dataframes for all traits -----
terr <- pv[pv$niche2 == "terrestrial",]
hemi <- pv[pv$niche2 == "hemi-epiphyte" ,]
epi <- pv[pv$niche2 == "epiphyte",]

#simple models ------
terr_mod <- lm(waterpot_tlp ~ elasticity, data=terr)
hemi_mod <- lm(waterpot_tlp ~ elasticity, data=hemi)
epi_mod <- lm(waterpot_tlp ~ elasticity, data=epi)


#plot bits-------

tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))
elasticity_lab <- "Elasticity"
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))

# windows(10,6)
jpeg(filename = "output/pv_traits_plots.jpeg",
     width = 10, height = 6, units = "in", res= 400)

par(oma=c(4,4,1,1), mfrow=c(1,2),mgp=c(2.5,.75,0),cex.lab=1.1)

par(mar=c(0,0,0,0))
plot(waterpot_tlp ~ osmotic_potential,data = pv, ylim=c(-2,0),xlim=c(-2,0),
     ylab="",xlab="Osmotic potential  (MPa)", type='n')
predline(om_mod_simple, lwd=2, lty=2)
points(waterpot_tlp ~ osmotic_potential,data = pv, col=trtcols2[niche2], 
       pch=16, cex=1.5)
legend("topleft", boxlabs, pch=16, col=trtcols, bty='n', inset = .01,cex=1.1)
axis(2,labels=FALSE)
mtext(side=2, at=-1, line=2.5,text=tlp_lab,
      xpd=TRUE, las=3, cex=1.1)
mtext(side=1, at=-1, line=2.5,text="Osmotic potential  (MPa)",
      xpd=TRUE, cex=1.1)
text(-.2,-1.95,"P < 0.001")

par(mar=c(0,0,0,0))
plot(waterpot_tlp ~ elasticity,data = pv, ylim=c(-2,0),xlim=c(0,65),
     ylab="",xlab="", type='n', yaxt='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(waterpot_tlp ~ elasticity,data = pv, col=trtcols2[niche2], 
       pch=16, cex=1.5)
mtext(side=1, at=32.5, line=2.5,text=elasticity_lab,
      xpd=TRUE, cex=1.1)
text(55,-1.95,"P = 0.007")

dev.off()  
