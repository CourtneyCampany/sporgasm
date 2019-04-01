
  ## data merge of stomatal data
library(doBy)

##stomatal density raw data from las cruces and la selva

stoden_ls <- read.csv("raw_data/stomata_density_ls.csv")
stoden_lc <- read.csv("raw_data/stomata_density_lc.csv")
niche <- read.csv("raw_data/species_niches.csv")


#la selva stomata density calculation:
  ##calculate a leaf disk mean, need to average multiple region counts first
  stoden_ls$sto_no <- with(stoden_ls, (sto1 + sto2 + sto3)/3)
  #scale sto_no to cm2 from FOV and mm2
  stoden_ls$sd_cm2 <- with(stoden_ls, (sto_no/fov_mm2 )* .01)
  stoden_ls$sd_mm2 <- with(stoden_ls, (sto_no/fov_mm2 ))

  stoden_ls_agg <- summaryBy(sd_cm2 +  sd_mm2 ~site + species + plant_no + 
                             disk + fov_mm2, FUN=mean, keep.names = TRUE,
                             data=stoden_ls)

#las cruces stomata desnity calculations:
fov_lc <- unique(stoden_lc$fov_mm2)
  stoden_lc_agg <- summaryBy(stomata_no ~ site + species + plant_no + disk +
                            fov_mm2,
                            FUN=mean, keep.names=TRUE,data=stoden_lc)
                            
  stoden_lc_agg$sd_cm2 <- with(stoden_lc_agg, (stomata_no/fov_mm2 )* .01)
  stoden_lc_agg$sd_mm2 <- with(stoden_lc_agg, (stomata_no/fov_mm2 ))
  
  
stom_density <- rbind(stoden_lc_agg[,c(1:5, 7:8)], stoden_ls_agg)
stomdensity2 <- merge(stom_density, niche, by=c("site", "species"), all=TRUE)

write.csv(stomdensity2, "calculated_data/stomata_density.csv", row.names = FALSE)
