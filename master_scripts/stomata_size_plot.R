#stomata size plots

#stomatal size from both sites
ss <- read.csv("raw_data/stomata_size_cec.csv")
  ss$plant_no <- gsub("[^0-9]", "", ss$individual)

#species habitats  
niche <- read.csv("raw_data/species_niches.csv")


ss2 <-  merge(ss, niche, by="genusspecies")
  ss2$niche2 <- gsub("climber", "hemi-epiphyte", ss2$niche)
  ss2$niche2 <- as.factor(ss2$niche2)
  
    #reorder from ground to canopy 
  ss2$niche2<-factor(ss2$niche2, 
                         levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  ss2$average_guardcell_width_um <- with(ss2, 
                            (guardcell_width1_um + guardcell_width1_um)/2)
  ss2$stomatal_size <- with(ss2, (guardcell_width1_um + guardcell_width1_um) *
                            guardcell_length_um)

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)
#write.csv(ss_agg, "calculated_data/stomata_size_means.csv", row.names = FALSE)

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange3"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
library(doBy)
trtcols2 <- c(alpha(trtcols[1], .5), alpha(trtcols[2], .5),alpha(trtcols[3], .5))
ss_lab <- expression(Stomatal~size~~(mu*m^2))
sl_lab <- expression(Stomatal~length~~(mu*m))
sw_lab <- expression(Stomatal~width[mean]~~(mu*m))

cld_ss <- c("a", "b", "b")
cld_s2 <- c("a", "b", "b")
  

##multiple panel plot of anatomy -------
# windows(10,12)

jpeg(filename = "output/stomatasize.jpeg",
     width = 10, height = 12, units = "in", res= 400)

par(mfrow=c(3,1),mgp=c(2.5,1,0),oma=c(5,5,1,1), cex.lab=1)

#stomata length
par(mar=c(0,0,0,0))
boxplot(guardcell_length_um ~ niche2, data=ss_agg, xaxt='n',ylim=c(0.03, .09),
        varwidth=TRUE,ylab="",border=trtcols)
axis(1, labels=FALSE, at=1:3)
stripchart(guardcell_length_um ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
mtext(side=2, at=.06, line=2.5,text=sl_lab,
      xpd=TRUE, las=3, cex=.9)

#stomatal width
par(mar=c(0,0,0,0))
boxplot(average_guardcell_width_um ~ niche2, data=ss_agg, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols, ylim=c(0.005, .022))
axis(1, labels=FALSE, at=1:3)
stripchart(average_guardcell_width_um ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
mtext(side=2, at=.015, line=2.5,text=sw_lab, 
      xpd=TRUE, las=3, cex=.9)

par(mar=c(0,0,0,0))
boxplot(stomatal_size ~ niche2, data=ss_agg, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols, ylim=c(0, .004))
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(stomatal_size ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=.00375, cld_ss)
mtext(side=2, at=.002, line=2.5,text=ss_lab, 
       xpd=TRUE, las=3, cex=.9)

dev.off()
