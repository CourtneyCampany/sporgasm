# stomatal size site merge
niche <- read.csv("raw_data/species_niches.csv")

ss_lc <- read.csv("raw_data/stomata_size_lc.csv")
ss_lc$genusspecies <- with(ss_lc, paste(genus, species, sep=""))

