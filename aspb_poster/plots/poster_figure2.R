
#stipe bivariate plots

source("aspb_poster/plots/plot_objects.R")
source("aspb_poster/plots/basic_functions.R")
source("aspb_poster/plots/ci_functions.R")
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

traits_poster <- droplevels(alldata3[alldata3$niche2 != "hemi-epiphyte",])

##separate habitat dataframes for all traits -----
terr <- traits_poster[traits_poster$niche2 == "terrestrial",]
epi <- traits_poster[traits_poster$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=terr)
epi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=epi)

#plot bits -----
cldxylemfrac <- c("a","b" )
boxlabs3 <- c("Terrestrial",  "Epiphyte")
trtcols_poster <- c("dodgerblue4","darkorange3")
trtcols_poster2 <- c(alpha(trtcols_poster[1], .7), alpha(trtcols_poster[2], .7))

jpeg(filename = "aspb_poster/plots/poster_figure2.jpeg",
     width = 10, height = 6, units = "in", res= 400)

# windows(12,5)
par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(6,4,1,1), cex.lab=1.15,cex.axis=1.15)

#xylem area
boxplot(xylemfrac ~ niche2, data=traits_poster[traits_poster$xylemfrac < .15,],
        xaxt='n',ylim=c(0, .2),
        border=trtcols_poster, varwidth=TRUE, outline=FALSE,xlab="",
        boxlwd=2,whisklwd=2,staplelwd=2, ylab=xylemfrac_lab)
axis(1, boxlabs3, at=1:2, cex.axis=1.15)
stripchart(xylemfrac ~ niche2, data=traits_poster[traits_poster$xylemfrac < .15,],
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,col= trtcols_poster2, xaxt='n', add=TRUE)
text(2.5, 0, "A", cex=1.25)
text(x=1:2, y=.18, cldxylemfrac, cex=1.15)

#mtext(fig2cap, 1, line=4, adj=0)
mtext(expression(paste("Figure 2. ", (A)~Box~plots~of~xylem~fraction~(total~xylem~area~(mm^2):lamina~area~(mm^2))~across~life~forms, ".")), 1, line=4, adj=0)
mtext("               (B) The capacity for greater total xylem area in terrestrial tropical ferns supports construction of large stipes.",
      1, line=5, adj=0)

#xylem stipe
plot(stipe_length_cm ~ xylem_area_mm2, data=traits_poster, type='n',
     ylab=stipe_lab, xlab=xylem_lab, xlim=c(0,.8), ylim=c(0,80))
axis(2, labels=FALSE, tcl=-.25)
axis(2, labels=FALSE, tcl=.25)
predline(terr_mod_xa, col=trtcols_poster[1], lwd=2, lty=2)
predline(epi_mod_xa, col=trtcols_poster[2], lwd=2, lty=2)
points(stipe_length_cm ~ xylem_area_mm2, data=traits_poster, col=trtcols_poster2[niche2],
     pch=16,cex=1.25)
legend("topleft", legend = boxlabs3, pch=16, col=trtcols_poster, bty="n", inset=.01,1.15)
text(.8, 0, "B", cex=1.25)
text(.85, 16, expression(paste(R[cond]^{"2"}," = "," 0.30")), 1.25)
text(.85, 9, expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)

dev.off()


#c13
# boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-38, -25),xlab="",
#         boxlwd=2, whisklwd=2,staplelwd=2,
#         ylab=c13lab,border=trtcols,  varwidth=TRUE, outline=FALSE)
# stripchart(d13C ~ niche2, data = c13dat,cex=1,
#            vertical = TRUE, method = "jitter", 
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
# axis(1, boxlabs, at=1:3, cex.axis=1)
# text(x=1:3, y=-25.5, cldc13, cex=1)
