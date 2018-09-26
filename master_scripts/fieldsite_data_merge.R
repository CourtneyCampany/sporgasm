#master merge of las cruces and la selva data sets

## fern morophology

pinnae_ls <- read.csv("raw_data/fern_traits_laselva.csv")
  pinnae_ls$date <- as.Date(pinnae_ls$date, format = "%d/%m/%Y", tz='UTC')

pinnae_lc <- read.csv("raw_data/fern_traits_lascruces.csv")
  pinnae_lc$date <- as.Date(pinnae_lc$date, format = "%m/%d/%Y", tz='UTC')
  pinnae_lc$lamina_length_cm <- with(pinnae_lc, frond_length_cm - stipe_length_cm)
  ##make averages, if only one measurement then use that measurement
  pinnae_lc$chl_mg_m2 <- with(pinnae_lc, (chl_1 + chl_2 + chl_3)/3)
  pinnae_lc$chl_mg_m2 <- ifelse(is.na(pinnae_lc$chl_mg_m2), 
                                pinnae_lc$chl_1, pinnae_lc$chl_mg_m2)
  
  pinnae_lc$cfr <- with(pinnae_lc, (cfr_1 + cfr_2 + cfr_3)/3)
  pinnae_lc$cfr <- ifelse(is.na(pinnae_lc$cfr), pinnae_lc$cfr_1, pinnae_lc$cfr)

traits <- merge(pinnae_lc, pinnae_ls, all=TRUE)
#drop the individual chlorphyll measurements
traits2 <- traits[1:10]

##select certain traits, then merge with niches
niche <- read.csv("raw_data/species_niches.csv")

traits3 <- merge(traits2, niche, all=TRUE)

write.csv(traits3, "calculated_data/fern_traits.csv", row.names = FALSE)
