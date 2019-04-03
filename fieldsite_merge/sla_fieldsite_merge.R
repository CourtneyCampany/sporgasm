#sla from la selva
drymass_ls <- read.csv("raw_data/fern_dryweights_laselva_clean.csv")

#there are gonna be some redos with pv curves and sla so index drymass 
#where sladrymass !is.na
sla_mass_ls <- drymass_ls[!(is.na(drymass_ls$sla_dry)),]
  sla_mass_ls$totalarea_cm2 <- with(sla_mass_ls, (sla_no_disks * size_mm2)/100)
  sla_mass_ls$sla <- with(sla_mass_ls, totalarea_cm2/sla_dry)
  
sla_ls <- sla_mass_ls[,c(1,3,4,6,10)]
  names(sla_ls)[2] <- "genusspecies"
  
#sla from las cruces
drymass_lc <- read.csv("raw_data/sla_dryweights_lc_clean.csv")  
#there are 2 sets that need to be split, with leaf punchs or with full area
#calculate SLA appropirately

leafpunch <- drymass_lc[!is.na(drymass_lc$no_disks), ]
  leafpunch$totalarea_cm2 <- with(leafpunch, (no_disks * disk_size_mm)/100)
  leafpunch$sla <- with(leafpunch, totalarea_cm2/sla_dry)

pinnule_sla <- drymass_lc[is.na(drymass_lc$no_disks), ] 
  pinnule_sla$sla <- with(pinnule_sla, pinnule_area_cm2/sla_dry)

#merge las cruces data

sla_lc <- rbind(leafpunch[,c(1:5,10)], pinnule_sla[,c(1:5,9)])
#drop the robin moran plant
sla_lc2 <- droplevels(sla_lc[!sla_lc$genus%in% "moranopteris",])
sla_lc2$genusspecies <- as.factor(paste(sla_lc2$genus, sla_lc2$species, sep="_"))

sla_lc3 <- sla_lc2[,c(1,4:7)]
  sla_lc3$plant_no <- as.factor(sla_lc3$plant_no)

##merge las cruces and la selva
sla_fern <- merge(sla_lc3, sla_ls, all=TRUE)
  names(sla_fern)[3] <- "dry_mass_g"
  names(sla_fern)[4] <- "sla_cm2g"
  names(sla_fern)[5] <- "species"
  sla_fern$site <- gsub("laselva", "la_selva", sla_fern$site)
  
niche <- read.csv("raw_data/species_niches.csv") 

sla_fern2 <- merge(sla_fern, niche, all=TRUE)                 
  sla_fern2$niche2 <- sla_fern2$niche
  sla_fern2$niche2 <- gsub("climber", "terrestrial", sla_fern2$niche2)
  sla_fern2$niche2 <- as.factor(sla_fern2$niche2)

write.csv(sla_fern2,"calculated_data/fern_sla.csv", row.names = FALSE)
