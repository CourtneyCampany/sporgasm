#xylem specific conductivity

##use kleaf data set and add to it
treat <- read.csv("raw_data/species_niches.csv")

leafK_ls <- read.csv("calculated_data/laselva_kmax.csv")
leafK_lc <- read.csv("calculated_data/lascruces_kmax.csv")

leafk <- rbind(leafK_lc, leafK_ls)
leafk2 <- merge(leafk, treat)

xylem <- read.csv("raw_data/stipe_xylem_area.csv")
xylem_no2017 <- xylem[xylem$year %in% c(2018, 2019),]


##need to double check some curve indiviudals matchups, search for missing
# test <- merge(leafk2, xylem_no2017, by=c("species","individual"), all=TRUE)

kleaf_xylem <- merge(leafk2, xylem_no2017, by=c("species","individual"))

                     