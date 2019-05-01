source("functions_packages/basic_functions.R")

library(mgcv) #for gam fits
library(scales)

pv_data <- read.csv("calculated_data/pv_curves_full.csv")

##plotting curves
gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
trtcols2 <- alpha(trtcols, .2)
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")
psi2lab <- expression(1/Psi~~(MPa^-1))

## trim weird data
pv_data2 <- pv_data[pv_data$rwc_100 < 15 & pv_data$rwc_100 >= 0,]
pv_data3 <- pv_data[pv_data$rwc_100 < 10 & pv_data$rwc_100 >= 0 &
                      pv_data$psi2 < 40,]

# windows()
par(mar=c(5,5,1,1))
plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,11),
     ylim=c(-1,15), ylab= psi2lab, xlab = "100-RWC ( %)")

legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)

points(psi2~rwc_100, data=pv_data3 , pch=16, col=trtcols2[niche2])

smoothplot(rwc_100, psi2, niche2,data=pv_data3, kgam=4, R="species",
           linecol=trtcols,pch="", add=TRUE)


##see with ggplots
library(ggplot2)
library(ggpubr)

theme_set(theme_bw())
theme_update(text = element_text(size=12),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             axis.text = element_text(colour = "black"),
             legend.justification=c(1,1), 
             legend.position=c(1,1),
             legend.background = element_blank(),
             legend.box.background = element_blank(),
             legend.key = element_blank(),
             legend.title = element_blank(),
             axis.text.x   = element_text(size=14),
             axis.text.y   = element_text(size=14),
             axis.title.y  = element_text(size=14),
             axis.title.x  = element_text(size=14),
)

#ggplot version
windows(7,7)
ggplot(data=pv_data3) + 
    geom_point(mapping = aes(x=rwc_100, y=psi2, col=niche2), alpha=1/5) +
    ylim(0,15) +
    xlim(0,10.5) +
    ylab(psi2lab) + 
    xlab("100-RWC ( %)") +
    geom_smooth(mapping = aes(x=rwc_100, y=psi2, col=niche2)) +
    scale_color_manual(name = "Relationship", 
                       values = c(trtcols[1], trtcols[2], trtcols[3]))
