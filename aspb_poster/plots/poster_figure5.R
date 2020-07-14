source("aspb_poster/plots/basic_functions.R")
source("aspb_poster/plots/plot_objects.R")

library(mgcv) #for gam fits
library(scales)

#pv parameters
pv <- read.csv("aspb_poster/plots/pv_curves2.csv")
    pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
    pv$niche2 <- as.factor(pv$niche2)
    #reorder from ground to canopy 
    pv$niche2<-factor(pv$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
    
pv_poster2 <- droplevels(pv[pv$niche2 != "hemi-epiphyte",])

##raw data points
pv_data <- read.csv("aspb_poster/plots/pv_curves_full.csv")
pv_data$niche2<-factor(pv_data$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

pv_poster <- droplevels(pv_data[pv_data$niche2 != "hemi-epiphyte",])

## trim weird data
pv_data3 <- pv_poster[pv_poster$rwc_100 < 15 & pv_poster$rwc_100 >= 0 &
                        pv_poster$psi2 < 40,]

pv_data4 <- pv_data3[complete.cases(pv_data3$psi2),]

#split dfr by habitat
terr <- pv_data4[pv_data4$niche2 == "terrestrial",]
epi <- pv_data4[pv_data4$niche2 == "epiphyte",]

#loess for pv global
loess_terr <- loess(psi2 ~ rwc_100, data=terr, span=.5)
terr$smooth_terr <- predict(loess_terr)

loess_epi <- loess(psi2 ~ rwc_100, data=epi, span=.5)
epi$smooth_epi <- predict(loess_epi)

#split dfr by habitat for panel 4
terr2 <- pv[pv$niche2 == "terrestrial",]
epi2 <- pv[pv$niche2 == "epiphyte",]

#loess for cap vs elas
loess_terr2 <- loess(capacitance_full ~ elasticity, data=terr2, span=.9)
terr2$smooth_terr2 <- predict(loess_terr2)
# pred_terr_se <-predict(loess_terr, se=T)

loess_epi2 <- loess(capacitance_full ~ elasticity, data=epi2, span=.5)
epi2$smooth_epi2 <- predict(loess_epi2)

##plot bits
psi2lab <- expression(1/Psi~~(MPa^-1))
op_lab <- expression(paste(Psi[o], "  (MPa)"))
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))
elas_lab <- expression(epsilon~~(MPa))
capft_lab <- expression(C[ft]~~(MPa^-1))
captlp_lab <- expression(C[TLP]~~(MPa^-1))

tlp_cld <- c("a","a" )
op_cld <- c("a","b" )
cap_cld <- c("a","b" )
boxlabs3 <- c("Terrestrial",  "Epiphyte")
trtcols_poster <- c("dodgerblue4","darkorange3")
trtcols_poster2 <- c(alpha(trtcols_poster[1], .7), alpha(trtcols_poster[2], .7))

#pv curve plots-------

jpeg(filename = "aspb_poster/plots/figure5_poster.jpeg",
      width = 10, height = 6, units = "in", res= 400)

par(mar=c(4,4,1,1), mfrow=c(1,2),mgp=c(2.5,.75,0),cex.lab=1.25,cex.axis=1.25)

#global pv curve
plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,15),
     ylim=c(0,20), ylab= psi2lab, xlab = "100-RWC ( %)")
points(psi2~rwc_100, data=pv_data4 , pch=16, col=trtcols_poster2[niche2],
       cex=1.25)
lines(x=terr[order(terr$rwc_100),"rwc_100"], 
      y=terr[order(terr$rwc_100),"smooth_terr"], type='l',
      lwd=4, lty=2, col=trtcols_poster[1])
lines(x=epi[order(epi$rwc_100),"rwc_100"], 
      y=epi[order(epi$rwc_100),"smooth_epi"], type='l',
      lwd=4, lty=2,col=trtcols_poster[2])
text(15, 0, "A", cex=1.5)
legend("topright", boxlabs3, pch=16, col=trtcols_poster, bty='n', inset = .01, cex=1.25)

#tlp
boxplot(waterpot_tlp ~ niche2, data=pv_poster2, xaxt='n',ylim=c(-2,0.1),
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",outline=FALSE,
        varwidth=TRUE,ylab=tlp_lab,
        border=trtcols_poster)
axis(1, boxlabs3, at=1:2, cex.axis=1.25)
stripchart(waterpot_tlp ~ niche2, data = pv_poster2,cex=1.25,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols_poster2, xaxt='n', add=TRUE)
text(x=1:2, y=0, tlp_cld, cex=1.25)
text(2.5, -2, "B", cex=1.5)

dev.off()

# ## osmotic potential
# boxplot(osmotic_potential ~ niche2, data=pv_poster2, xaxt='n',
#         boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
#         varwidth=TRUE,ylab=op_lab,ylim=c(-2,0.1),
#         border=trtcols_poster)
# axis(1, boxlabs3, at=1:2, cex.axis=1.25)
# stripchart(osmotic_potential ~ niche2, data = pv_poster2,
#            vertical = TRUE, method = "jitter",cex=1.25,
#            pch = 16,  col= trtcols_poster2, xaxt='n', add=TRUE)
# text(x=1:2, y=0, op_cld, cex=1.25)
# text(2.5, -2, "C", cex=1.5)



