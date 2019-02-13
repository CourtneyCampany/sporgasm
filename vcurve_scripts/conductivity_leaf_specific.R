library(dplyr)

#read in all vcurves

laselva <- read.csv("calculated_data/laselva_vcurves.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="la_selva")

lascruces <- read.csv("calculated_data/lascruces_vcurves.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="las_cruces")

#merge sites
fern_plc <- merge(laselva, lascruces, all=TRUE)
# add species and habitat info
species_info <- read.csv("raw_data/species_niches.csv")

fern_plc_habitat <- merge(fern_plc, species_info, by=c("genusspecies", "site"))
#have only one climber to reclassify as terrestrial
fern_plc_habitat$niche2 <- fern_plc_habitat$niche 
fern_plc_habitat$niche2 <- gsub("climber", "terrestrial", 
                                fern_plc_habitat$niche2) %>% 
  as.factor()