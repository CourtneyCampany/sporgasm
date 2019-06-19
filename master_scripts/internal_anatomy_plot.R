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

cldstomata <- c("a","b","b" )

# jpeg(filename = "output/figure3_anatomy.jpeg",
#      width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#sd
boxplot(sd_mm2 ~ niche2, data=stomata_noout, xaxt='n',ylim=c(0, 170),
        varwidth=TRUE,xlab="",
        ylab=expression(Stomatal~density~~(mm^2)),border=trtcols,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(sd_mm2 ~ niche2, data = stomata_noout,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=160, cldstomata)
text(.5, 170, "A", cex=1.25)

#sd vs ss
plot(sd_mm2 ~ stomatal_size , data=stomata_noout, ylim=c(0,171),
     xlim=c(0.0003, .0029), xlab=ss_lab, ylab=sd_lab, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stomatal_size,data=stomata_noout,pch=16,  col= trtcols2[niche2],
       cex=1.25)
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
text(0.0003, 170, "B", cex=1.25)

# dev.off()
