#stomata size plots

#only las cruces for now

ss <- read.csv("raw_data/stomata_size_lc.csv")
  ss$genusspecies <- with(ss, paste(genus, species, sep=""))
  ss$plant_no <- gsub("[^0-9]", "", ss$individual)

ss2 <- ss[, -c(2:3)]  #remove genus and species codes
  
niche <- read.csv("raw_data/species_niches.csv")

ss3 <-  merge(ss2, niche, by=c("genusspecies", "site"))
  ss3$niche2 <- gsub("climber", "terrestrial", ss3$niche)
  ss3$niche2 <- as.factor(ss3$niche2)

  #reorder from ground to canopy 
  ss3$niche2<-factor(ss3$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + guardcell_width1_um +
                          guardcell_width2_um  ~ site + species + plant_no 
                          + niche2,
                          data=ss3, FUN=mean, keep.names = TRUE)

ss_agg$guard_width <- with(ss_agg, (guardcell_width1_um + guardcell_width1_um)/2)



#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange1"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))

# windows(7,10)
par(mfrow=c(2,1),mgp=c(2.5,1,0),oma=c(4,4,1,1), cex.lab=1)

#stomata length
par(mar=c(0,0,0,0))
boxplot(guardcell_length_um ~ niche2, data=ss_agg, xaxt='n',ylim=c(0.03, .07),
        varwidth=TRUE,ylab="",border=trtcols)
axis(1, labels=FALSE, at=1:3)
title(main="Las Cruces only", line=-1)
stripchart(guardcell_length_um ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
mtext(side=2, at=.05, line=2.5,text="Guard cell length  (um)",
      xpd=TRUE, las=3, cex=.9)

#stomatal width
par(mar=c(0,0,0,0))
boxplot(guard_width ~ niche2, data=ss_agg, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols, ylim=c(0.005, .022))
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(guard_width ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
mtext(side=2, at=.011, line=2.5,text="Guard cell width  (um)", 
      xpd=TRUE, las=3, cex=.9)
