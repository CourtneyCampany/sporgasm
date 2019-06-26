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


## trim weird data
pv_data3 <- pv_data[pv_data$rwc_100 < 15 & pv_data$rwc_100 >= 0 &
                      pv_data$psi2 < 40,]

pv_data4 <- pv_data3[complete.cases(pv_data3$psi2),]

#split dfr by habitat
terr <- pv_data4[pv_data4$niche2 == "terrestrial",]
hemi <- pv_data4[pv_data4$niche2 == "hemi-epiphyte",]
epi <- pv_data4[pv_data4$niche2 == "epiphyte",]

#loess for pv global
loess_terr <- loess(psi2 ~ rwc_100, data=terr, span=.5)
terr$smooth_terr <- predict(loess_terr)
# pred_terr_se <-predict(loess_terr, se=T)

loess_hemi<- loess(psi2 ~ rwc_100, data=hemi, span=.5)
hemi$smooth_hemi <- predict(loess_hemi)

loess_epi <- loess(psi2 ~ rwc_100, data=epi, span=.5)
epi$smooth_epi <- predict(loess_epi)


#split dfr by habitat for panel 4
terr2 <- pv[pv$niche2 == "terrestrial",]
hemi2 <- pv[pv$niche2 == "hemi-epiphyte",]
epi2 <- pv[pv$niche2 == "epiphyte",]

#loess for cap vs elas
loess_terr2 <- loess(capacitance_full ~ elasticity, data=terr2, span=.9)
terr2$smooth_terr2 <- predict(loess_terr2)
# pred_terr_se <-predict(loess_terr, se=T)

loess_hemi2<- loess(capacitance_full ~ elasticity, data=hemi2, span=.5)
hemi2$smooth_hemi2 <- predict(loess_hemi2)

loess_epi2 <- loess(capacitance_full ~ elasticity, data=epi2, span=.5)
epi2$smooth_epi2 <- predict(loess_epi2)

##plot bits
psi2lab <- expression(1/Psi~~(MPa^-1))
op_lab <- expression(paste(Psi[o], "  (MPa)"))
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))
elas_lab <- expression(epsilon~~(MPa))
capft_lab <- expression(C[ft]~~(MPa^-1))
captlp_lab <- expression(C[TLP]~~(MPa^-1))

tlp_cld <- c("a","a","a" )
op_cld <- c("a","a","b" )
cap_cld <- c("a","ab","b" )

#pv curve plots-------

# jpeg(filename = "output/plot_4_pvparams.jpeg",
#       width = 12, height = 12, units = "in", res= 400)

par(mar=c(4,4,1,1), mfrow=c(2,2),mgp=c(2.5,.75,0),cex.lab=1.1)

#global pv curve
plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,15),
     ylim=c(0,20), ylab= psi2lab, xlab = "100-RWC ( %)")
points(psi2~rwc_100, data=pv_data4 , pch=16, col=trtcols3[niche2],
       cex=1.25)
lines(x=terr[order(terr$rwc_100),"rwc_100"], 
      y=terr[order(terr$rwc_100),"smooth_terr"], type='l',
      lwd=4, lty=2, col=trtcols[1])
lines(x=hemi[order(hemi$rwc_100),"rwc_100"], 
      y=hemi[order(hemi$rwc_100),"smooth_hemi"], type='l',
      lwd=4, lty=2, col=trtcols[2])
lines(x=epi[order(epi$rwc_100),"rwc_100"], 
      y=epi[order(epi$rwc_100),"smooth_epi"], type='l',
      lwd=4, lty=2,col=trtcols[3])
text(15, 0, "A", cex=1.25)
legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)

#tlp
boxplot(waterpot_tlp ~ niche2, data=pv, xaxt='n',ylim=c(-2,0.1),
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
        varwidth=TRUE,ylab=tlp_lab,
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(waterpot_tlp ~ niche2, data = pv,cex=1.25,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=0, tlp_cld)
text(3.5, -2, "B", cex=1.25)


## osmotic potential
boxplot(osmotic_potential ~ niche2, data=pv, xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
        varwidth=TRUE,ylab=op_lab,ylim=c(-2,0.1),
        border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(osmotic_potential ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=0, op_cld)
text(3.5, -2, "C", cex=1.25)

#capacitance vs elas

plot(capacitance_full ~ elasticity, data=pv, type='n',xlab=elas_lab,
     ylab=capft_lab, xlim=c(0, 65))
points(capacitance_full ~ elasticity, data=pv , pch=16, 
       col=trtcols3[niche2],
       cex=1.25)
lines(x=terr2[order(terr2$elasticity),"elasticity"], 
      y=terr2[order(terr2$elasticity),"smooth_terr2"], 
      lwd=4, lty=2, col=trtcols[1])
lines(x=hemi2[order(hemi2$elasticity),"elasticity"], 
      y=hemi2[order(hemi2$elasticity),"smooth_hemi2"], 
      lwd=4, lty=2, col=trtcols[2])
lines(x=epi2[order(epi2$elasticity),"elasticity"], 
      y=epi2[order(epi2$elasticity),"smooth_epi2"], 
      lwd=4, lty=2, col=trtcols[3])
text(65, .01, "D", cex=1.25)

par(fig=c(.7, .95, 0.25,0.45), mar=c(0,2.5,0,0),new=T, cex=1, las=1,
    cex.axis=.7, cex.lab=.7, tcl=-.25,mgp=c(1.5,.5,0))
boxplot(capacitance_zero. ~ niche2, data=pv, xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,ylim=c(0, 1.25),
        varwidth=TRUE,ylab=captlp_lab,outline=FALSE,xlab="",
        border=trtcols)
axis(1, labels=FALSE, at=1:3)
mtext(boxlabs, 1, at=1:3,line=.05, cex=.7)
stripchart(capacitance_zero. ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",cex=.8,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=1.2, cap_cld)

# dev.off()
