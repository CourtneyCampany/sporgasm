source("functions_packages/basic_functions.R")
treat <- read.csv("raw_data/species_niches.csv")

#1/psi plots with gam

pv <- read.csv("raw_data/pv_curves_ls_clean.csv")


pv_leafmass <- read.csv("calculated_data/pv_curves2.csv")
drymass_ls <- pv_leafmass[pv_leafmass$site == "la_selva",
                          c("species", "plant_no", "dry_mass")]
#merge just dry mass to pv curves

pv2 <- merge(pv, drymass_ls)

pv2$psi_mpa <- pv2$psi_bars/-10
pv2$psi2 <- -1/pv2$psi_mpa
pv2$leafwater <- pv2$wet_weight_g - pv2$dry_mass

pv2$curve_id <- as.factor(with(pv2, paste(species, plant_no, sep="-")))

##need to split into a list and run a function to calculate RWC
library(data.table)
pv_list <- split(pv2, pv2$curve_id)

rwc_function <- function(x){
  swc_slope <- sd(x$leafwater)/sd(x$psi_mpa)
  swc <- mean(x$leafwater) - swc_slope * mean(x$psi_mpa)
  
  x$rwc <- x$leafwater / swc
  x$rwc_perc <- 100*x$rwc
  x$rwc_100 <- 100-x$rwc_perc
  return(x)
}

pv_list2 <- lapply(pv_list, rwc_function)
library(dplyr)

#return to big dataframe
pv3 <- bind_rows(pv_list2)

#should be able to plot

pv4 <- pv3[pv3$psi2 < 40,]
pv5 <- merge(pv4, treat, by="species")
  pv5$niche2 <- gsub("climber", "hemi-epiphyte", pv5$niche)
  pv5$niche2 <- as.factor(pv5$niche2)

#reorder from ground to canopy 
  pv5$niche2<-factor(pv5$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
write.csv(pv5,"calculated_data/pv_curves_full.csv", row.names = FALSE)

##plotting curves
  gradient <- colorRampPalette(c("forestgreen","darkorange1"))
  palette(gradient(3))
  trtcols <- palette(gradient(3))
  boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

# windows()
par(mar=c(5,5,1,1))
smoothplot(rwc_100, psi2, niche2,data=pv5, kgam=5, R="species",
          linecol=trtcols,pch="", xlim=c(0,20),
          ylim=c(0,6.5), ylab= "1/Psi  (MPa-1)", xlab = "100-RWC ( %)")
legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)

# points(psi2~rwc_100, data=pv5 , col=trtcols[niche2])

