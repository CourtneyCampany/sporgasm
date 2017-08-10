#plotting objects

makelabels <- read.csv("raw_data/fern_traits.csv")


niche_lab <- c("Climber", "Epiphyte", "Hemi-epiphyte", "Terresetrial")

n_terr <- length(unique(makelabels[makelabels$niche =="terrestrial", "species"]))
n_climb <- length(unique(makelabels[makelabels$niche =="climber", "species"]))
n_epi <- length(unique(makelabels[makelabels$niche =="epiphyte", "species"]))
n_hemi <- length(unique(makelabels[makelabels$niche =="hemi-epiphyte", "species"]))

niche_count <- c(n_climb, n_epi, n_hemi, n_terr)
niche_lab2 <- paste("n=", niche_count, sep = "")

