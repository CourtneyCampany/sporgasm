niche <- read.csv("raw_data/species_niches.csv")

laselva <- read.csv("raw_data/pv_traits_ls.csv")

lascruces <- read.csv("raw_data/pv_traits_lc.csv")


pvcurves <- rbind(laselva, lascruces)

#no dipati
pvcurves2 <- merge(pvcurves, niche, by=c("species", "site"))

#save only the values with full curves (some just have leaf area)

pvcurves3 <- pvcurves2[complete.cases(pvcurves2$SWC),]

write.csv(pvcurves3, "calculated_data/pv_curves.csv", row.names = FALSE)



