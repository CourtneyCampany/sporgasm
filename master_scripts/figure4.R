source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)
library(mgcv) #for gam fits

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

terr <- droplevels(stomata_noout[stomata_noout$niche2 == "terrestrial",])
hemi <- droplevels(stomata_noout[stomata_noout$niche2 == "hemi-epiphyte" ,])
epi <- droplevels(stomata_noout[stomata_noout$niche2 == "epiphyte",])

terr2 <- terr[complete.cases(terr$stomatal_size) & complete.cases(terr$sd_mm2),]
hemi2 <- hemi[complete.cases(hemi$stomatal_size) & complete.cases(hemi$sd_mm2),]
epi2 <- epi[complete.cases(epi$stomatal_size) & complete.cases(epi$sd_mm2),]

terr_mod <- lm(stomatal_size ~ sd_mm2 , data=terr)
hemi_mod <- lm(stomatal_size ~ sd_mm2 , data=hemi)
epi_mod <- lm(stomatal_size ~ sd_mm2 , data=epi)

fernmod <- lm(stomatal_size ~ sd_mm2, stomata_noout)

cldstomata <- c("a","b","b" ) #from stomata density stats
cldsize <- c("a","b","b" )

#loess for sd vs ss
# loess_terr <- loess(stomatal_size ~ sd_mm2, data=terr2, span=5)
# terr2$smooth_terr <- predict(loess_terr)
# 
# loess_hemi<- loess(stomatal_size ~ sd_mm2, data=hemi2, span=5)
# hemi2$smooth_hemi <- predict(loess_hemi)
# 
# loess_epi <- loess(stomatal_size ~ sd_mm2, data=epi2, span=5)
# epi2$smooth_epi2 <- predict(loess_epi)

##panel plot-------

# jpeg(filename = "output/figure3_anatomy.jpeg",
#      width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.15)

#sd
boxplot(sd_mm2 ~ niche2, data=stomata_noout, xaxt='n',ylim=c(0, 170),
        varwidth=TRUE,xlab="",
        ylab=expression(Stomatal~density~~(mm^2)),border=trtcols,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:3, cex.axis=1.15)
stripchart(sd_mm2 ~ niche2, data = stomata_noout,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=165, cldstomata, cex=1.15)
text(.5, 170, "A", cex=1.25)

#stomatal sze

boxplot(stomatal_size ~ niche2, data=stomata_noout, xaxt='n',
        varwidth=TRUE,xlab="",outline=FALSE,ylim=c(200,2700),
        ylab=ss_lab,border=trtcols,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:3, cex.axis=1.15)
stripchart(stomatal_size ~ niche2, data = stomata_noout,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=2600, cldsize, cex=1.25)
text(.5, 2700, "B", cex=1.25)

##below is removed SD vs SS
# plot(stomatal_size ~ sd_mm2, data=stomata_noout, 
#      ylab=ss_lab, xlab=sd_lab, type='n')
# predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
# predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
# predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
# points(stomatal_size ~ sd_mm2, data=stomata_noout, pch=16, col= trtcols2[niche2],
#        cex=1.25)
# legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
# text(8, 3300, "B", cex=1.25)
# 
# lines(x=terr2[order(terr2$sd_mm2),"sd_mm2"],
#       y=terr2[order(terr2$sd_mm2),"smooth_terr"],
#       lwd=4, lty=2, col=trtcols[1])
# lines(x=hemi2[order(hemi2$sd_mm2),"sd_mm2"],
#       y=hemi2[order(hemi2$sd_mm2),"smooth_hemi"],
#       lwd=4, lty=2, col=trtcols[2])
# lines(x=epi2[order(epi2$sd_mm2),"sd_mm2"],
#       y=epi2[order(epi2$sd_mm2),"smooth_epi"],
#       lwd=4, lty=2, col=trtcols[3])

# text(130,1700, expression(paste(R[cond]^{"2"}," = "," 0.22")))
# text(130,1500, expression(paste(R[marg]^{"2"}," = "," 0.90")))
# dev.off()
