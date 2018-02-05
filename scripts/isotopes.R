iso <- read.csv("raw_data/fern_isotopes.csv")

source("master_scripts/plot_objects.R")
niche <- read.csv("raw_data/species_niches.csv")

iso <- merge(iso, niche, by="species")

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(d13C ~ niche, data = iso, xaxt='n',outline=FALSE,
        ylab=expression(Frond~{delta}^13*C~~('\211')))
axis(1, niche_lab, at=1:4, cex=1.1)


boxplot(d15N ~ niche, data = iso, xaxt='n',outline=FALSE,
        ylab=expression(Frond~{delta}^15*N~~('\211')))
axis(1, niche_lab, at=1:4, cex=1.1)