#plotting objects

makelabels <- read.csv("aspb_poster/plots/species_niches.csv")


niche_lab <- c("Climber", "Epiphyte", "Hemi-epiphyte", "Terrestrial")
niche_lab_noclimb <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")
niche_lab_goodorder <- c("Terrestrial","Hemi-epiphyte","Epiphyte")

n_terr <- length(unique(makelabels[makelabels$niche =="terrestrial", "species"]))
n_climb <- length(unique(makelabels[makelabels$niche =="climber", "species"]))
n_epi <- length(unique(makelabels[makelabels$niche =="epiphyte", "species"]))
n_hemi <- length(unique(makelabels[makelabels$niche =="hemi-epiphyte", "species"]))

niche_count <- c(n_climb, n_epi, n_hemi, n_terr)
niche_lab2 <- paste("n=", niche_count, sep = "")


##plot axis labels
lamina_lab <- expression(Lamina~area~~(cm^2))
frond_lab <- "Frond length  (cm)"
stipe_lab <- "Stipe length  (cm)"
c13lab <-expression(paste(delta^{13}, "C (\u2030)"))
k_lab <- expression(Conductivity~~(mg~mm~KPa^-1~s^-1))
slalab<- expression(SLA[TNC~free]~~(m^2~g^-1))
lmalab <- expression(LMA~~(g~m^-2))
nitrolab <- expression(Leaf~Nitrogen~~(g~g^-1))
n15lab <-expression(paste(delta^{15}, "N (\u2030)"))
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")
boxlabs2 <- c("Terrestrial", "Hemi-\nepiphyte", "Epiphyte")
ss_lab <- expression(Stomatal~size~~(um^2))
sl_lab <- expression(Stomatal~length~~(um))
sw_lab <- expression(Stomatal~width[mean]~~(um))
sd_lab <- expression(Stomatal~density~~(stomata~mm^-2))
xylem_lab <- expression(Total~xylem~area[stipe]~~(mm^-2))
xylem_lab2 <- expression(Total~xylem~area[stipe]~~(um^-2))
wp_lab <- expression(Psi[midday]~~(MPa))
lma_lab <- expression(LMA~~(g~m^-2))
cap_lab <- expression(C[tlp]~~(units))
veinlab <- expression(Vein~Density~~(mm~mm^-2))
xylemfrac_lab <- expression(Xylem~fraction~x~10^4)


#colors
# gradient <- colorRampPalette(c("dodgerblue4","darkorange3"))
# palette(gradient(3))
# trtcols <- palette(gradient(3))

trtcols <- c("dodgerblue4","yellow4","darkorange3")
library(scales)
trtcols2 <- c(alpha(trtcols[1], .7), alpha(trtcols[2], .7),alpha(trtcols[3], .7))
trtcols3 <- c(alpha(trtcols[1], .4), alpha(trtcols[2], .4),alpha(trtcols[3], .4))


#phylogeny bits

# fernnode_per <- c(100,100,95,75,100,97,100,88,100,90,100,100,
#                    100,100,100,71,100,79,96,98,76,100,100,100,
#                    100,100,100,100,100,71,100,98,100,100,100,100)

fernnode_perc <- c(100,100,51,100,99,96,84,100,92,100,100,100,98,100,100,100,
                   72,100,79,96,98,73,100,100,100,100,100,100,100,100,75,
                   100,98,100,100,100,100)