source("functions_packages/basic_functions.R")
library(mgcv)
library(scales)
pv5 <- read.csv("calculated_data/pv_curves_full.csv")

##plotting curves
gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

# windows()
par(mar=c(5,5,1,1))
smoothplot(rwc_100, psi2, niche2,data=pv5, kgam=5, R="species",
           linecol=trtcols,pch="", xlim=c(0,20),
           ylim=c(0,6.5), ylab= "1/Psi  (MPa-1)", xlab = "100-RWC ( %)")
legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)

# points(psi2~rwc_100, data=pv5 , col=trtcols[niche2])

