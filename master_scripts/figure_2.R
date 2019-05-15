source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

###c13 data---------
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
  leafchem$niche2 <- gsub("climber", "hemi-epiphyte", leafchem$niche)
  leafchem$niche2 <- as.factor(leafchem$niche2)
  #reorder from ground to canopy 
  leafchem$niche2<-factor(leafchem$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


#stomata data (densitry and size) -------

#stomata size
stodens <- read.csv("calculated_data/stomata_density.csv")
  stodens$niche2 <- gsub("climber", "hemi-epiphyte", stodens$niche)
  stodens$niche2 <- as.factor(stodens$niche2)
  #reorder from ground to canopy 
  stodens$niche2<-factor(stodens$niche2, 
                         levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ site + species + plant_no + niche2 + 
                          genusspecies,data=stodens, FUN=mean, keep.names = TRUE)


#stomata size
stomsize <- read.csv("raw_data/stomata_size_cec.csv")
  stomsize$plant_no <- gsub("[^0-9]", "", stomsize$individual)

#species habitat treatments  
niche <- read.csv("raw_data/species_niches.csv")


stomsize2 <-  merge(stomsize, niche, by="genusspecies")
  stomsize2$niche2 <- gsub("climber", "hemi-epiphyte", stomsize2$niche)
  stomsize2$niche2 <- as.factor(stomsize2$niche2)
  #reorder from ground to canopy 
  stomsize2$niche2<-factor(stomsize2$niche2,
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  stomsize2$average_guardcell_width_um <- with(stomsize2, (guardcell_width1_um + 
                                               guardcell_width1_um)/2)
  stomsize2$stomatal_size <- with(stomsize2, (guardcell_width1_um + 
                             guardcell_width1_um) * guardcell_length_um)

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=stomsize2, FUN=mean, keep.names = TRUE)


#merge density and size
stomata <- merge(sd_agg, ss_agg, by=c("species", "plant_no","niche2"))
#drop species outlier for stom density
stomata_noout <- droplevels(stomata[!stomata$genusspecies == "oleart",])

library(lme4)
library(MuMIn)
stom_mod <- lm(sd_mm2 ~ stomatal_size * niche2 * site, data=stomata_noout)
# stom_mod2 <- lmer(sd_mm2 ~ stomatal_size + (1|species), data=stomata_noout)
Anova(stom_mod, type=3)
summary(stom_mod)

##multi panel plot
c13dat <- leafchem[!leafchem$genusspecies == "bleschi",]
cldc13 <- c("a","a","b" )

#simple models of stomata
terr_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "terrestrial",])
hemi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "hemi-epiphyte",])
epi_mod <- lm(sd_mm2 ~ stomatal_size, data=stomata_noout[stomata_noout$niche2 == "epiphyte",])


jpeg(filename = "output/figure2.jpeg",
     width = 10, height = 7, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)

#stomata
plot(sd_mm2 ~ stomatal_size , data=stomata_noout,ylim=c(0,145),
     xlim=c(0.0003, .0029), xlab=ss_lab, ylab=sd_lab, type='n')
predline(terr_mod, col=trtcols[1], lwd=2, lty=2)
predline(hemi_mod, col=trtcols[2], lwd=2, lty=2)
predline(epi_mod, col=trtcols[3], lwd=2, lty=2)
points(sd_mm2 ~ stomatal_size,data=stomata_noout,pch=16,  col= trtcols2[niche2])
legend("topright", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)

#c13
boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-40, -25),
        ylab=c13lab,border=trtcols,  varwidth=TRUE, outline=FALSE)
stripchart(d13C ~ niche2, data = c13dat,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex.axis=1.1)
text(x=1:3, y=-25.5, cldc13)

dev.off()

                 