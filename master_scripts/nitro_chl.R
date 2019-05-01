source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")

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


##add chlorophyll
traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

chloro <- traits[complete.cases(traits$chl_mg_m2),]  
#reorder from ground to canopy 
chloro$niche2<-factor(chloro$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
##same data as stats, drop outliers and bad data for elaher
chloro4 <- chloro[chloro$chl_mg_m2 < 800 & !chloro$genusspecies == "elaher",]


nitro2 <- merge(nitro, chloro4, all = TRUE)
nitro2$lma_g_m2 <- with(nitro2, 1/(sla_cm2g/10000))
nitro2$nitro_area <- with(nitro2,lma_g_m2 * (n_perc/100))
nitro2$chl_mg_cm2 <- nitro2$chl_mg_m2/1000