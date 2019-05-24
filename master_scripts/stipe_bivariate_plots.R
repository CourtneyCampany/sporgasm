#stipe bivariate plots

source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(visreg)
library(plotrix)

plot(lamina_area_cm2~ stipe_length_cm, data=alldata, xlab=stipe_lab,
     ylab=sd_lab,  col=trtcols2[niche2],
     pch=16,cex=1.25)

##PCA using only means of traits, 
## pvcuurves are not from sample individuals as morphology stuff

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=terr)
hemi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=hemi)
epi_mod_xa <- lm(xylem_area_mm2 ~ stipe_length_cm, data=epi)

terr_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=terr)
hemi_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=hemi)
epi_mod_xala <- lm(log10(lamina_area_cm2) ~ log10(xylem_area_mm2), data=epi)

terr_mod_sd <- lm(sd_mm2~ stipe_length_cm, data=terr)
hemi_mod_sd<- lm(sd_mm2 ~ stipe_length_cm, data=hemi)
epi_mod_sd<- lm(sd_mm2 ~ stipe_length_cm, data=epi)

#plot bits -----

stipecld <- c("a", "ab", "b")


jpeg(filename = "output/figure3.jpeg",
     width = 12, height = 5, units = "in", res= 400)  

par(mfrow=c(1,3),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

# # stipe length
# boxplot(stipe_length_cm ~ niche2, data=alldata, ylim=c(0, 82),xaxt='n',
#         boxlwd=2,whisklwd=2,staplelwd=2,
#         ylab = stipe_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
# axis(1, boxlabs, at=1:3, cex.axis=1.1)
# stripchart(stipe_length_cm ~ niche2, data = alldata,
#            vertical = TRUE, method = "jitter",cex=1.25,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
# text(x=1:3, y=80, stipecld)
# text(3.5, 0, "A", cex=1.25)


#stipe length and xa (maybe scales differently, little to no stipe)
plot(xylem_area_mm2 ~ stipe_length_cm, data=alldata, type='n',
     xlab=stipe_lab, ylab=xylem_lab, ylim=c(0,1.1), xlim=c(0,80))
predline(terr_mod_xa, col=trtcols[1], lwd=2, lty=2)
predline(epi_mod_xa, col=trtcols[3], lwd=2, lty=2)
points(xylem_area_mm2 ~ stipe_length_cm, data=alldata, col=trtcols2[niche2],
     pch=16,cex=1.25)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)


#stipe length and stomata density
plot(sd_mm2~ stipe_length_cm, data=alldata, xlab=stipe_lab,type='n',
     ylab=sd_lab, ylim=c(0,150), xlim=c(0,80))
predline(terr_mod_sd, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod_sd, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod_sd, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stipe_length_cm, data=alldata, col=trtcols2[niche2],
       pch=16,cex=1.25)
# legend("bottomright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

#lamina area and xa (some outliers, if remove do they have same slope?)
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

dev.off()
