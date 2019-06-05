niche <- read.csv("raw_data/species_niches.csv")

#mergin all traits

#huber by niche
huber <- read.csv("calculated_data/xylem_area_huber.csv")

#morphology traits
traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
allferns <- merge(traits, huber, all=TRUE)

#stomata density
sd <- read.csv("calculated_data/stomata_density_means.csv") 
sd2 <- merge(sd, niche)

ss <- read.csv("calculated_data/stomata_size_means.csv")
ss2 <- merge(ss, niche)

stom <- merge(ss2, sd2, all=TRUE)


allferns2 <- merge(allferns, stom, all=TRUE)
  
#pressure volume curves
pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)
  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  pv$plant_no <- as.integer(pv$plant_no)
  
allferns3<- merge(allferns2, pv, all=TRUE)
  
sla <- read.csv("calculated_data/fern_sla.csv")
  #reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

allferns4<- merge(allferns3, sla, all=TRUE)  
  
#leaf chemistry
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
 
allferns5 <- merge(allferns4, leafchem[,c(1:5,8,10:11)], all=TRUE) 

write.csv(allferns5, "calculated_data/ferns_traits_complete.csv", row.names = FALSE)





