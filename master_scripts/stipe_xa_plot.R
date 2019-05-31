#stipe bivariate plots

source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]

#huber by niche
xylem <- read.csv("calculated_data/xylem_area_huber.csv")
  xylem$niche2<-factor(xylem$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  xylem2 <- xylem[xylem$xylem_area_mm2 < 0.8,]

##separate habitat dataframes for all traits -----
terr <- alldata2[alldata2$niche2 == "terrestrial",]
hemi <- alldata2[alldata2$niche2 == "hemi-epiphyte" ,]
epi <- alldata2[alldata2$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=terr)
hemi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=hemi)
epi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=epi)

#plot bits -----
cldxylem <- c("a","ab","b" )
cldhuber <- c("a", "b", "b")

# jpeg(filename = "output/stipe_xylem.jpeg",
#      width = 12, height = 5, units = "in", res= 400)  

par(mfrow=c(1,2),mgp=c(2.5,.75,0), oma=c(4,4,1,1), cex.lab=1.1)

#xylem area
par(mar=c(0,0,0,0))
boxplot(xylem_area_mm2 ~ niche2, data=xylem2,xaxt='n',ylim=c(0, .85),
        border=trtcols, varwidth=TRUE, outline=FALSE,xlab="",
        boxlwd=2,whisklwd=2,staplelwd=2, ylab="")
axis(1, boxlabs, at=1:3, cex.axis=1.1)
stripchart(xylem_area_mm2 ~ niche2, data = xylem2,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(0.5, 0.85, "A", cex=1.25)
text(x=1:3, y=.775, cldxylem)
mtext(xylem_lab, 2, line=2.5, cex=1.1)

#xylem stipe
par(mar=c(0,0,0,0))
plot(xylem_area_mm2 ~ stipe_length_cm, data=alldata2, type='n',yaxt='n',
     xlab="", ylab=xylem_lab, ylim=c(0,.85), xlim=c(0,80))
axis(2, labels=FALSE, tcl=-.25)
axis(2, labels=FALSE, tcl=.25)
predline(terr_mod_xa, col=trtcols[1], lwd=2, lty=2)
predline(epi_mod_xa, col=trtcols[3], lwd=2, lty=2)
points(xylem_area_mm2 ~ stipe_length_cm, data=alldata2, col=trtcols2[niche2],
     pch=16,cex=1.25)
legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
text(0, .85, "B", cex=1.25)
mtext(stipe_lab, 1, line=2.5, cex=1.1)



# #xylem_lamina
# with(alldata2, plot(log10(lamina_area_cm2) ~ log10(xylem_area_mm2),
#                    ylab=lamina_lab, xlab=xylem_lab,axes=FALSE,
#                    pch=16, col=trtcols2[niche2],cex=1.25))
# magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
# # legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
# ablineclip(terr_mod_xala, x1=log10(0.00252200), 
#            x2=log10(0.89649758),
#            col=trtcols[1], lwd=3, lty=2)
# ablineclip(hemi_mod_xala, x1=log10(0.00140100), 
#            x2=log10(0.65607611),
#            col=trtcols[2], lwd=3, lty=2)
# ablineclip(epi_mod_xala, x1=log10(0.00939000), 
#            x2=log10(0.25124324),
#            col=trtcols[3], lwd=3, lty=2)
# 
# text(log10(0.0015), log10(1900), "D", cex=1.25)


# dev.off()
