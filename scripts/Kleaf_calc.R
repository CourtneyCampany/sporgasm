##use Kmax from Vcurves and add leaf area for Kleaf

treat <- read.csv("raw_data/species_niches.csv")


##las selva -------

kmax_ls <- read.csv("calculated_data/laselva_kmax.csv")
leafarea_ls <- read.csv("raw_data/vcurve_leafarea_laselva_2019.csv")

kleaf_ls <- merge(kmax_ls, leafarea_ls, by=c("genusspecies", "individual"))
kleaf_ls$kmax_leaf <- with(kleaf_ls, K/leafarea_cm2)

kleaf2_ls <- merge(kleaf_ls, treat, by="genusspecies")
kleaf2_ls$niche2 <- gsub("climber", "hemi-epiphyte", kleaf2_ls$niche)
kleaf2_ls$niche2 <- as.factor(kleaf2_ls$niche2)
  
  #reorder from ground to canopy 
kleaf2_ls$niche2<-factor(kleaf2_ls$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


##las cruces -------
kmax_lc <- read.csv("calculated_data/lascruces_kmax.csv")
leafarea_lc <- read.csv("raw_data/vcurve_leafarea_lascruces_2018.csv")
  
kleaf_lc <- merge(kmax_lc, leafarea_lc, by=c("genusspecies", "individual"))
kleaf_lc$kmax_leaf <- with(kleaf_lc, K/leafarea_cm2)

kleaf2_lc <- merge(kleaf_lc, treat, by="genusspecies")
  kleaf2_lc$niche2 <- gsub("climber", "hemi-epiphyte", kleaf2_lc$niche)
  kleaf2_lc$niche2 <- as.factor(kleaf2_lc$niche2)
  
    #reorder from ground to canopy 
  kleaf2_lc$niche2<-factor(kleaf2_lc$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
    
##merge sites and write ------------
  
  kleaf <- rbind(kleaf2_ls, kleaf2_lc)
  
write.csv(kleaf, "calculated_data/kleaf.csv", row.names = FALSE)
  
