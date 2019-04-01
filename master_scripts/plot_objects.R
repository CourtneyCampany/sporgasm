#plotting objects

makelabels <- read.csv("raw_data/species_niches.csv")


niche_lab <- c("Climber", "Epiphyte", "Hemi-epiphyte", "Terrestrial")
niche_lab_noclimb <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")

n_terr <- length(unique(makelabels[makelabels$niche =="terrestrial", "species"]))
n_climb <- length(unique(makelabels[makelabels$niche =="climber", "species"]))
n_epi <- length(unique(makelabels[makelabels$niche =="epiphyte", "species"]))
n_hemi <- length(unique(makelabels[makelabels$niche =="hemi-epiphyte", "species"]))

niche_count <- c(n_climb, n_epi, n_hemi, n_terr)
niche_lab2 <- paste("n=", niche_count, sep = "")

gradient <- colorRampPalette(c("darkgreen", "orange"))
palette(gradient(4))
nichcols <- palette(gradient(4))


##plot axis labels
lamina_lab <- expression(Lamina~area~~(cm^2))
frond_lab <- "Frond length  (cm)"
stipe_lab <- "Stipe length  (cm)"
c13lab <-expression(paste(delta^{13}, "C (\u2030)"))
k_lab <- expression(Conductivity~~(mg~mm~KPa^-1~s^-1))
slalab<- expression(SLA[TNC~free]~~(m^2~g^-1))
lmalab <- expression(LMA[TNC~free]~~(g~cm^-2))
nitrolab <- expression(Leaf~Nitrogen~~(g~g^-1))
n15lab <-expression(paste(delta^{15}, "N (\u2030)"))
