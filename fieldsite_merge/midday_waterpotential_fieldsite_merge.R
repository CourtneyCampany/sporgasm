##merge water potential

wp_ls <- read.csv("raw_data/waterpotential_ls.csv")
wp_lc <- read.csv("raw_data/waterpotential_lc.csv")

treat <- read.csv("raw_data/species_niches.csv")


midday <- rbind(wp_lc, wp_ls)
midday_wp <- merge(midday, treat, by=c("genusspecies", "site"))
midday_wp$niche2 <- gsub("climber", "hemi-epiphyte", midday_wp$niche)
midday_wp$niche2 <- as.factor(midday_wp$niche2)
                   
#reorder from ground to canopy 
midday_wp$niche2<-factor(midday_wp$niche2, 
                 levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

write.csv(midday_wp, "calculated_data/waterpotential_middday.csv", row.names = FALSE)

