
library(data.table)
library(plyr)

#species names and treatments
treat <- read.csv("raw_data/species_niches.csv")

##clean pv curves parameters and their leaf traits (both sites)
pv_leafmass <- read.csv("calculated_data/pv_curves2.csv")

#need to prepare raw data to make 1/psi plots 

#function to calculate relative leaf water content --------
rwc_function <- function(x){
  swc_slope <- sd(x$leafwater)/sd(x$psi_mpa)
  swc <- mean(x$leafwater) - swc_slope * mean(x$psi_mpa)
  
  x$rwc <- x$leafwater / swc
  x$rwc_perc <- 100*x$rwc
  x$rwc_100 <- 100-x$rwc_perc
  return(x)
}



#la selva data-------------
pv_ls <- read.csv("raw_data/pv_curves_ls_clean.csv")
drymass_ls <- pv_leafmass[pv_leafmass$site == "la_selva",
                          c("species", "plant_no", "dry_mass")]

#merge just dry mass to pv curves

pv2_ls <- merge(pv_ls, drymass_ls)
  pv2_ls$psi_mpa <- pv2_ls$psi_bars/-10
  pv2_ls$psi2 <- -1/pv2_ls$psi_mpa
  pv2_ls$leafwater <- pv2_ls$wet_weight_g - pv2_ls$dry_mass
  pv2_ls$curve_id <- as.factor(with(pv2_ls, paste(species, plant_no, sep="-")))

##need to split into a list and run a function to calculate RWC

pv_list_ls <- split(pv2_ls, pv2_ls$curve_id)
pv_list2_ls <- lapply(pv_list_ls, rwc_function)

#return to big dataframe
pv3_ls <- bind_rows(pv_list2_ls)



#las cruces data-------------
  
pv_lc <- read.csv("raw_data/pv_curves_lc_clean.csv")
drymass_lc <- pv_leafmass[pv_leafmass$site == "las_cruces",
                            c("species", "plant_no", "dry_mass")]  
  
#merge just dry mass to pv curves

pv2_lc <- merge(pv_lc, drymass_lc, all=TRUE)
  pv2_lc$psi_mpa <- pv2_lc$psi_bars/-10
  pv2_lc$psi2 <- -1/pv2_lc$psi_mpa
  pv2_lc$leafwater <- pv2_lc$wet_weight_g - pv2_lc$dry_mass
  pv2_lc$curve_id <- as.factor(with(pv2_lc, paste(species, plant_no, sep="-"))) 

##need to split into a list and run a function to calculate RWC
pv_list_lc <- split(pv2_lc, pv2_lc$curve_id)
pv_list2_lc <- lapply(pv_list_lc, rwc_function)
  
#return to big dataframe
pv3_lc <- bind_rows(pv_list2_lc)
  


##merge both sites------
pv_data <- rbind(pv3_ls, pv3_lc)

#add treatments, re-order factors 
pv_data2 <- merge(pv_data, treat, by=c("species", "site"))
  pv_data2$niche2 <- gsub("climber", "hemi-epiphyte", pv_data2$niche)
  pv_data2$niche2 <- as.factor(pv_data2$niche2)
  
  #reorder from ground to canopy 
  pv_data2$niche2<-factor(pv_data2$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  

##merge both raw data sets and write
write.csv(pv_data2,"calculated_data/pv_curves_full.csv", row.names = FALSE)

