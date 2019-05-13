## figure 2 (stomatal traits) panel figure 
source("master_scripts/plot_objects.R")

#stomata density data --------------------
stodens <- read.csv("calculated_data/stomata_density.csv")
stodens$niche2 <- gsub("climber", "hemi-epiphyte", stodens$niche)
stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + genusspecies,
                          data=stodens, FUN=mean, keep.names = TRUE)


#stomatal size data --------

ss <- read.csv("raw_data/stomata_size.csv")
  ss$plant_no <- gsub("[^0-9]", "", ss$individual)

#species habitats  
niche <- read.csv("raw_data/species_niches.csv")


ss2 <-  merge(ss, niche, by="genusspecies")
  ss2$niche2 <- gsub("climber", "hemi-epiphyte", ss2$niche)
  ss2$niche2 <- as.factor(ss2$niche2)
  #reorder from ground to canopy 
  ss2$niche2<-factor(ss2$niche2,
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  ss2$average_guardcell_width_um <- with(ss2, (guardcell_width1_um + 
                                           guardcell_width1_um)/2)
  ss2$stomatal_size <- with(ss2, (guardcell_width1_um + guardcell_width1_um) *
                     guardcell_length_um)

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)


#c13 data -----
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "hemi-epiphyte", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)

  #reorder from ground to canopy 
  leafchem$niche2<-factor(leafchem$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#plot bits-------
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")

gradient <- colorRampPalette(c("forestgreen","darkorange3"))
palette(gradient(3))
trtcols <- palette(gradient(3))
library(scales)
trtcols2 <- c(alpha(trtcols[1], .7), alpha(trtcols[2], .7),alpha(trtcols[3], .7))

#stomata plot bits
sd_new <- droplevels(sd_agg[!sd_agg$genusspecies == "oleart",])
#drop oleart as we do in model
cldsd <- c("a","b","b" )
cldss <- c("a","b","b" )
ss_lab <- expression(Stomatal~size~~(mu*m^2))
#c13 plot bits
c13dat <- leafchem[!leafchem$genusspecies == "bleschi",]
cldc13 <- c("a","a","b" )

# figure 2 plot ------
jpeg(filename = "output/figure2.jpeg",
     width = 10, height = 10, units = "in", res= 400)  

par(mfrow=c(2,2),mgp=c(2.5,1,0), mar=c(5,5,1,1), cex.lab=1)

#stomatal density
boxplot(sd_mm2 ~ niche2, data=sd_new, xaxt='n',ylim=c(0, 162),varwidth=TRUE,
        ylab=expression(Stomatal~density~~(mm^2)),border=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(sd_mm2 ~ niche2, data = sd_new,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=157, cldsd)

#stomatal density
boxplot(stomatal_size ~ niche2, data=ss_agg, xaxt='n',varwidth=TRUE,
        ylab=ss_lab,border=trtcols, ylim=c(0, .004))
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart(stomatal_size ~ niche2, data = ss_agg,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
text(x=1:3, y=.00375, cldss)

# c13
boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-40, -25),
        ylab=c13lab,border=trtcols,  varwidth=TRUE, outline=FALSE)
stripchart(d13C ~ niche2, data = c13dat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex=1.1)
text(x=1:3, y=-26, cldc13)



# dev.off()
