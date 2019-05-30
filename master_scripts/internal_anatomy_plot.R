source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)


alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#simple models of stomata
#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

terr_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "terrestrial",])
hemi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "hemi-epiphyte",])
epi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "epiphyte",])

terr_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=terr)
hemi_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=hemi)
epi_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=epi)

#huber by niche
huber <- read.csv("calculated_data/xylem_area_huber.csv")
huber$niche2<-factor(huber$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
xylem2 <- huber[huber$xylem_area_mm2 < 0.8,]

cldstomata <- c("a","b","b" )
cldxylem <- c("a","ab","b" )
cldhuber <- c("a", "b", "b")

#plot
# jpeg(filename = "output/figure3_anatomy.jpeg",
#      width = 10, height = 10, units = "in", res= 400)

par(mfrow=c(2,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#sd
boxplot(sd_mm2 ~ niche2, data=stomata_noout, xaxt='n',ylim=c(0, 170),
        varwidth=TRUE,
        ylab=expression(Stomatal~density~~(mm^2)),border=trtcols,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(sd_mm2 ~ niche2, data = stomata_noout,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=160, cldstomata)
text(.5, 170, "A", cex=1.25)

#sd vs ss
plot(sd_mm2 ~ stomatal_size , data=stomata_noout, ylim=c(0,171),
     xlim=c(0.0003, .0029), xlab=ss_lab, ylab=sd_lab, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stomatal_size,data=stomata_noout,pch=16,  col= trtcols2[niche2])
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
text(0.0003, 160, "B", cex=1.25)

#xylem area
boxplot(xylem_area_mm2 ~ niche2, data=xylem2,xaxt='n',ylim=c(0, .85),
        border=trtcols, varwidth=TRUE, outline=FALSE,
        boxlwd=2,whisklwd=2,staplelwd=2, ylab=xylem_lab)
axis(1, boxlabs, at=1:3, cex.axis=1.1)
stripchart(xylem_area_mm2 ~ niche2, data = xylem2,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(.5, 0.8, "C", cex=1.25)
text(x=1:3, y=.775, cldxylem)

#huber
with(alldata, plot(log10(lamina_area_cm2) ~ log10(xylem_area_mm2),
                   ylab=lamina_lab, xlab=xylem_lab,axes=FALSE,
                   pch=16, col=trtcols2[niche2],cex=1.25))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
# legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
ablineclip(terr_mod_xala, x1=log10(0.00252200), 
           x2=log10(0.89649758),
           col=trtcols[1], lwd=3, lty=2)
ablineclip(hemi_mod_xala, x1=log10(0.00140100), 
           x2=log10(0.65607611),
           col=trtcols[2], lwd=3, lty=2)
ablineclip(epi_mod_xala, x1=log10(0.00939000), 
           x2=log10(0.25124324),
           col=trtcols[3], lwd=3, lty=2)

text(log10(0.0015), log10(1900), "D", cex=1.25)

# dev.off()
