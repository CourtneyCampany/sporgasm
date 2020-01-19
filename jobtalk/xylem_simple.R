##xylem figure for job talk


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

##separate habitat dataframes for all traits -----
terr <- alldata3[alldata3$niche2 == "terrestrial",]
hemi <- alldata3[alldata3$niche2 == "hemi-epiphyte" ,]
epi <- alldata3[alldata3$niche2 == "epiphyte",]

#simple models ------
terr_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=terr)
hemi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=hemi)
epi_mod_xa <- lm(stipe_length_cm ~ xylem_area_mm2, data=epi)

#plot bits -----
cldxylem <- c("a","b","b" )

 jpeg(filename = "jobtalk/xylem.jpeg",
      width = 7, height = 7, units = "in", res= 400)  

# windows(12,5)
 par(mgp=c(3,1,0), mar=c(5,5,1,1), cex.lab=1.25)

#xylem area
boxplot(xylem_area_mm2 ~ niche2, data=alldata3,xaxt='n',ylim=c(0, .85),
        border=trtcols, varwidth=TRUE, outline=FALSE,xlab="",
        boxlwd=2,whisklwd=2,staplelwd=2, ylab=xylem_lab)
axis(1, boxlabs, at=1:3, cex.axis=1.25)
stripchart(xylem_area_mm2 ~ niche2, data = alldata3,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
dev.off()