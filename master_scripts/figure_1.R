## figure 1 (morphology and chemistry) panel figure 
source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")


#traits data -----
traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##separate habitat dataframes for all traits -----
terr <- traits[traits$niche2 == "terrestrial" & 
                 complete.cases(traits$stipe_length_cm),]
hemi <- traits[traits$niche2 == "hemi-epiphyte" &
                 complete.cases(traits$stipe_length_cm),]
epi <- traits[traits$niche2 == "epiphyte"&
                complete.cases(traits$stipe_length_cm),]

#log model fits for allometry plotting
terr_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + .1) ,data=terr)
hemi_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + .1) , data=hemi)
epi_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + .1) ,data=epi)
fronddat <- traits[-203,] ##loses one outlier so same as stats


# chemistry and lma data -----
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
terr2 <- droplevels(nitro[nitro$niche2 == "terrestrial", ])
hemi2 <- droplevels(nitro[nitro$niche2 == "hemi-epiphyte", ])
epi2 <- droplevels(nitro[nitro$niche2 == "epiphyte", ])

# terr_mod2 <- lm(n_perc ~ lma_g_m2, data=terr2[terr2$lma_g_m2 < 600,])
# hemi_mod2 <- lm(n_perc ~ lma_g_m2, data=hemi2[hemi2$lma_g_m2 < 600,])
# epi_mod2 <- lm(n_perc ~ lma_g_m2, data=epi2[epi2$lma_g_m2 < 600,])

terr_mod2 <- lm(n_perc ~ lma_g_m2, data=terr2)
hemi_mod2 <- lm(n_perc ~ lma_g_m2, data=hemi2)
epi_mod2 <- lm(n_perc ~ lma_g_m2, data=epi2)

#plot bits-------
library(magicaxis)
library(plotrix)
stipecld <- c("a", "ab", "b")
cldlma <- c("a", "ab", "b")
lma_lab <- expression(LMA~~(g~m^-2))


#plot ----------
# windows(12,8)

jpeg(filename = "output/figure1.jpeg",
      width = 10, height = 10, units = "in", res= 400)  

par(mfrow=c(2,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

# allometry
with(fronddat, plot(log10(lamina_area_cm2) ~ log10(stipe_length_cm),
                    xlab=stipe_lab, ylab=lamina_lab,axes=FALSE,
                    pch=16, col=trtcols2[niche2],cex=1.25,
                    xlim=c(-0.05,2.05),
                    ylim=c(1, 3.5)))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
ablineclip(terr_mod, x1=log10(min(terr$stipe_length_cm+.1)), 
           x2=log10(max(terr$stipe_length_cm+1)),
           col=trtcols[1], lwd=3, lty=2)
ablineclip(hemi_mod, x1=log10(min(hemi$stipe_length_cm+.1)), 
           x2=log10(max(hemi$stipe_length_cm)),
           col=trtcols[2], lwd=3, lty=2)
ablineclip(epi_mod, x1=log10(min(epi$stipe_length_cm+.1)), 
           x2=log10(max(epi$stipe_length_cm)),
           col=trtcols[3], lwd=3, lty=2)
text(log10(250), log10(10), "A", cex=1.25)

# stipe length
boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 82),xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,
        ylab = stipe_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex.axis=1.1)
stripchart(stipe_length_cm ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=80, stipecld)
text(3.5, 0, "B", cex=1.25)


##lma

boxplot(lma_g_m2 ~ niche2, data=nitro,xaxt='n',ylim=c(0,630),
        boxlwd=2, whisklwd=2,staplelwd=2,
        ylab=lma_lab, outline=FALSE, border=trtcols, varwidth=TRUE)
axis(1, boxlabs, at=1:3, cex.axis=1.1)
stripchart(lma_g_m2 ~ niche2, data = nitro,cex=1.25,
           vertical = TRUE, method = "jitter",
           pch=16, col=trtcols2,
           xaxt='n', add=TRUE) 
text(x=1:3, y=600, cldlma)
text(3.5, 0, "C", cex=1.25)

#lma v nitro
plot(n_perc ~ lma_g_m2, data=nitro,
     ylim=c(0,6), xlim=c(0,600),
     ylab="Foliar Nitrogen (%)",xlab=lmalab, type='n')
predline(hemi_mod2, col=trtcols[2], lwd=2, lty=2)
predline(terr_mod2, col=trtcols[1], lwd=2, lty=2)
predline(epi_mod2, col=trtcols[3], lwd=2, lty=2)
points(n_perc ~ lma_g_m2, data=nitro[nitro$lma_g_m2 < 600,], 
       pch=16, col=trtcols2[niche2],cex=1.25)
text(600, 0, "D", cex=1.25)

# legend("topleft", legend = boxlabs, pch=21, pt.bg=trtcols, bty="n", inset=.01)
# text(455,.2,"LMA x Niche, P < 0.001")

dev.off()
