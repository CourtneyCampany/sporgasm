stodens <- read.csv("calculated_data/stomata_density.csv")
stodens$niche2 <- gsub("climber", "hemi-epiphyte", stodens$niche)
stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + 
                          genusspecies,data=stodens, FUN=mean, keep.names = TRUE)



ss <- read.csv("raw_data/stomata_size.csv")
  ss$plant_no <- gsub("[^0-9]", "", ss$individual)

#species habitats  
niche <- read.csv("raw_data/species_niches.csv")


ss2 <-  merge(ss, niche, by="genusspecies")
  ss2$niche2 <- gsub("climber", "hemi-epiphyte", ss2$niche)
  ss2$niche2 <- as.factor(ss2$niche2)
  #reorder from ground to canopy 
  ss2$niche2<-factor(ss2$niche2,
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  ss2$average_guardcell_width_um <- with(ss2, (guardcell_width1_um + 
                                                 guardcell_width1_um)/2)
  ss2$stomatal_size <- with(ss2, (guardcell_width1_um + guardcell_width1_um) *
                              guardcell_length_um)

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)



stomata <- merge(sd_agg, ss_agg, by=c("species", "plant_no","niche2"))

gradient <- colorRampPalette(c("forestgreen","darkorange3"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .7), alpha(trtcols[2], .7),alpha(trtcols[3], .7))


windows()

jpeg(filename = "output/stomata_regression.jpeg",
     width = 10, height = 7, units = "in", res= 400)
plot(stomatal_size ~ sd_mm2, data=stomata, col=niche2, pch=16)
dev.off()

                 