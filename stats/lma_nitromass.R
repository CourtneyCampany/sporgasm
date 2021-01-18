source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

# chemistry and lma data -----
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

cldlma <- c("a", "ab", "b")

#c13 and lma (eventually merge this with above)
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <-  with(alldata, 1/(sla_cm2g/10000))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#models
# terr_mod_13c <- lm(d13C ~ lma, data=terr)
# hemi_mod_13c <- lm(d13C ~ lma, data=hemi)
# epi_mod_13c <- lm(d13C ~ lma, data=epi) #no epi

c13dat <- alldata[!alldata$genusspecies == "bleschi",]
cldc13 <- c("a","a","b") 


##lma plots ------ 
library(magicaxis)
library(scales)
glopnet <- read.csv("calculated_data/glopnet.csv")
glopfern <- glopnet[glopnet$GF == "F",]
globcol <- alpha("grey50", .2)

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
with(nitro, plot(log10(n_perc) ~ log10(lma_g_m2), ylim=c(-.65,.9),xlim=c(1,3),
                     xlab = lmalab,
                     ylab = "Foliar Nitrogen (%)",
                     axes=FALSE, type='n'))
with(glopnet, points(log.Nmass ~ log.LMA,pch=16, col=globcol, cex=1.25))
with(nitro, points(log10(n_perc)~ log10(lma_g_m2),pch=16, col=trtcols2[niche2],cex=1.25))
# with(glopfern, points(log.Nmass ~ log.LMA,pch=16, col=globcol, cex=1.25))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)



#glopnet


plot(log.Nmass ~ log.LMA, data=glopnet)
