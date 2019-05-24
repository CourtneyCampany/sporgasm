source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

library(mgcv) #for gam fits
library(scales)

#pv parameters
pv <- read.csv("calculated_data/pv_curves2.csv")
    pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
    pv$niche2 <- as.factor(pv$niche2)
    #reorder from ground to canopy 
    pv$niche2<-factor(pv$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##raw data points
pv_data <- read.csv("calculated_data/pv_curves_full.csv")
pv_data$niche2<-factor(pv_data$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##plotting curves
psi2lab <- expression(1/Psi~~(MPa^-1))
op_lab <- expression(paste(Psi[o], "  (MPa)"))
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))

## trim weird data
pv_data2 <- pv_data[pv_data$rwc_100 < 15 & pv_data$rwc_100 >= 0,]
pv_data3 <- pv_data[pv_data$rwc_100 < 10 & pv_data$rwc_100 >= 0 &
                      pv_data$psi2 < 40,]

terr <- pv_data3[pv_data3$niche2 == "terrestrial",]
hemi <- pv_data3[pv_data3$niche2 == "hemi-epiphyte",]
epi <- pv_data3[pv_data3$niche2 == "epiphyte",]

# windows()
# par(mar=c(5,5,1,1))
# plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,11),
#      ylim=c(-1,15), ylab= psi2lab, xlab = "100-RWC ( %)")
# 
# legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)
# 
# points(psi2~rwc_100, data=pv_data3 , pch=16, col=trtcols2[niche2])
# 
# smoothplot(rwc_100, psi2, data=terr, kgam=4, R="species",
#            linecol=trtcols[1],pch="", add=TRUE)
# 
# smoothplot(rwc_100, psi2, data=hemi, kgam=.5, R="species",
#            linecol=trtcols[2],pch="", add=TRUE)
# 
# smoothplot(rwc_100, psi2, data=epi, kgam=4, R="species",
#            linecol=trtcols[3],pch="", add=TRUE)


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
             axis.text.x   = element_text(size=12),
             axis.text.y   = element_text(size=12),
             axis.title.y  = element_text(size=12),
             axis.title.x  = element_text(size=12),
)

pv_data4 <- pv_data3[complete.cases(pv_data3$niche2),]


# test <- ggplot(data=pv_data4) + 
#     geom_point(mapping = aes(x=rwc_100, y=psi2, col=niche2), alpha=1/5) +
#     ylim(0,15) +
#     xlim(0,10.5) +
#     ylab(psi2lab) + 
#     xlab("100-RWC ( %)") +
#     geom_smooth(mapping = aes(x=rwc_100, y=psi2, col=niche2)) +
#     scale_color_manual(name = "Relationship", 
#                        values = c(trtcols[3], trtcols[2], trtcols[1]))
# 
# #global curves
# print(test)

jpeg(filename = "output/plot_4.jpeg",
     width = 12, height = 5, units = "in", res= 400)

par(oma=c(4,4,1,1), mfrow=c(1,3),mgp=c(2.5,.75,0),cex.lab=1.1)

ggplot(data=pv_data4) + 
    geom_point(mapping = aes(x=rwc_100, y=psi2, col=niche2), alpha=1/5,
               size=2) +
    ylim(0,15) +
    xlim(0,10.5) +
    ylab(psi2lab) + 
    xlab("100-RWC ( %)") +
    geom_smooth(mapping = aes(x=rwc_100, y=psi2, col=niche2)) +
    scale_color_manual(name = "Relationship", 
                       values = c(trtcols[3], trtcols[2], trtcols[1]))

#tlp
boxplot(waterpot_tlp ~ niche2, data=pv, xaxt='n',ylim=c(-2,0.1),
        varwidth=TRUE,ylab=tlp_lab,
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(waterpot_tlp ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)


## osmotic potential
boxplot(osmotic_potential ~ niche2, data=pv, xaxt='n',
        varwidth=TRUE,ylab=op_lab,ylim=c(-2,0.1),
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(osmotic_potential ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
dev.off()

