
#stipe bivariate plots

source("master_scripts/plot_objects.R")
source("functions_packages/basic_functions.R")
source("functions_packages/ci_functions.R")
library(plotrix)

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
alldata$id <- paste(alldata$genusspecies, alldata$plant_no, sep="-")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]
#remove outliers detected in stats (lomjap-hemi)
alldata3 <- alldata2[! alldata2$id  %in% c("lomjap-4","lomjap-3","lomjap-6"),]

alldata3$xylemfrac <- (alldata3$xylem_area_mm2 / (alldata3$lamina_area_cm2 * 100))*10000
#xylemfrac is unitless standardized stem xylem measurement (huber value)
#multiple x 100000 for ease of view


##separate habitat dataframes for all traits -----
terr <- alldata3[alldata3$niche2 == "terrestrial",]
hemi <- alldata3[alldata3$niche2 == "hemi-epiphyte" ,]
epi <- alldata3[alldata3$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=terr)
hemi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=hemi)
epi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=epi)

#plot bits -----
cldxylemfrac <- c("a","b","b" )

# jpeg(filename = "output/stipe_xylem.jpeg",
#      width = 12, height = 5, units = "in", res= 400)  

# windows(12,5)
par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.15)


#xylem area
boxplot(xylemfrac ~ niche2, data=alldata3[alldata3$xylemfrac < .15,],
        xaxt='n',ylim=c(0, .2),
        border=trtcols, varwidth=TRUE, outline=FALSE,xlab="",
        boxlwd=2,whisklwd=2,staplelwd=2, ylab="Xylem Fraction")
axis(1, boxlabs, at=1:3, cex.axis=1.15)
stripchart(xylemfrac ~ niche2, data=alldata3[alldata3$xylemfrac < .15,],
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 17,col= trtcols2, xaxt='n', add=TRUE)
text(3.5, 0.85, "A", cex=1.25)
text(x=1:3, y=.18, cldxylemfrac, cex=1.15)

#xylem stipe
plot(stipe_length_cm ~ xylem_area_mm2, data=alldata3, type='n',
     ylab=stipe_lab, xlab=xylem_lab, xlim=c(0,.8), ylim=c(0,80))
axis(2, labels=FALSE, tcl=-.25)
axis(2, labels=FALSE, tcl=.25)
predline(terr_mod_xa, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod_xa, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod_xa, col=trtcols[3], lwd=2, lty=2)
points(stipe_length_cm ~ xylem_area_mm2, data=alldata3, col=trtcols2[niche2],
     pch=16,cex=1.25)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
text(0.8, 80, "B", cex=1.25)
text(.7, 10, expression(paste(R[cond]^{"2"}," = "," 0.30")))
text(.7, 3, expression(paste(R[marg]^{"2"}," = "," 0.88")))


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
