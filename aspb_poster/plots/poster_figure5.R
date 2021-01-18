source("aspb_poster/plots/plot_objects.R")
source("aspb_poster/plots/basic_functions.R")
source("aspb_poster/plots/ci_functions.R")
library(plotrix)


alldata <- read.csv("aspb_poster/plots/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#drop species outlier for stom density
stomata_noout <- droplevels(alldata[!alldata$genusspecies == "oleart",])

stomata_poster <- droplevels(stomata_noout[stomata_noout$niche2 != "hemi-epiphyte",])

cldstomata <- c("a","b" ) #from stomata density stats
cldsize <- c("a","b" )

boxlabs3 <- c("Terrestrial",  "Epiphyte")
trtcols_poster <- c("dodgerblue4","darkorange3")
trtcols_poster2 <- c(alpha(trtcols_poster[1], .7), alpha(trtcols_poster[2], .7))

##panel plot-------

jpeg(filename = "aspb_poster/plots/figure5_poster.jpeg",
     width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2,.75,0), mar=c(5,4,1,1), cex.lab=1.15,cex.axis=1.15)

#sd
boxplot(sd_mm2 ~ niche2, data=stomata_poster, xaxt='n',ylim=c(0, 170),
        varwidth=TRUE,xlab="",
        ylab=sd_lab,border=trtcols_poster,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs3, at=1:2, cex.axis=1.15)
stripchart(sd_mm2 ~ niche2, data = stomata_poster,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols_poster2, xaxt='n', add=TRUE) 
text(x=1:2, y=165, cldstomata, cex=1.15)
text(2.5, 0, "A", cex=1.25)

mtext("Figure 5. (A) Box plots of stomatal density across life forms.", 1, line=3, adj=0)
mtext("               (B) Box plots of stomatal size across life forms. ",
      1, line=4, adj=0)

#stomatal sze

boxplot(stomatal_size ~ niche2, data=stomata_poster, xaxt='n',
        varwidth=TRUE,xlab="",outline=FALSE,ylim=c(200,2800),
        ylab=ss_lab,border=trtcols_poster,
        boxlwd=2,whisklwd=2,staplelwd=2)
axis(1, boxlabs3, at=1:2, cex.axis=1.15)
stripchart(stomatal_size ~ niche2, data = stomata_poster,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols_poster2, xaxt='n', add=TRUE) 
text(x=1:2, y=2700, cldsize, cex=1.25)
text(2.5, 200, "B", cex=1.25)

dev.off()
