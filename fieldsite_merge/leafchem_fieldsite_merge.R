#format isotope data
m2 <- function(x) mean(x, na.rm=TRUE)

niche <- read.csv("raw_data/species_niches.csv")

#laselva
iso_ls <- read.csv("raw_data/fern_isotopes_ls.csv")
iso_ls2 <- merge(iso_ls, niche, by=c("species", "site"))
  #determine elemental%, sample is in mg, C,N is in ug
  iso_ls2$c_perc <- with(iso_ls2, 100*(1-((sample_mg-(c_ug/1000))/sample_mg)))
  iso_ls2$n_perc <- with(iso_ls2, 100*(1-((sample_mg-(n_ug/1000))/sample_mg)))
iso_ls3 <- iso_ls2[,c(1:4,6,8:13)]

#las cruces
iso_lc <- read.csv("raw_data/fern_isotopes_lc.csv")
iso_lc2 <-  merge(iso_lc, niche, by=c("species", "site"))

fern_isotopes <- rbind(iso_ls3, iso_lc2)


write.csv(fern_isotopes, "calculated_data/leaf_chemistry.csv", row.names = FALSE)
