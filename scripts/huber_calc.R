## calculate huber value

xylem <- read.csv("raw_data/stipe_xylem_area_clean.csv")

treat <- read.csv("raw_data/species_niches.csv")


xylem2 <- merge(xylem, treat)
  # 1 missing species (pec pec)
  xylem2$niche2 <- gsub("climber", "hemi-epiphyte", xylem2$niche)
  xylem2$niche2 <- as.factor(xylem2$niche2)
    #reorder from ground to canopy 
  xylem2$niche2<-factor(xylem2$niche2, 
                          levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##huber value is xylem area / leaf area it supports
xylem2$ lamina_area_mm2 <- 100 * xylem2$lamina_area_cm2
xylem2$xylem_area_mm2 <- xylem2$xylem_area_um2 * 10^-6
#calculate huber value with units synced
xylem2$huber <- with(xylem2, xylem_area_mm2/lamina_area_mm2)

write.csv(xylem2, "calculated_data/xylem_area_huber.csv", row.names = FALSE)


