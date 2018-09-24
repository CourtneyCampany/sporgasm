source("master_scripts/plot_objects.R")

## fern morophology

pinnae_ls <- read.csv("raw_data/fern_traits_laselva.csv")
pinnae_lc <- read.csv("raw_data/fern_traits_laselva.csv")

niche <- read.csv("raw_data/species_niches.csv")

##select certain traits, then merge with niches

area <- pinnae[, c(1:4, 8, 11)]
length <- pinnae[complete.cases(pinnae), c(1:7, 11)]

#drymass
drymass <- read.csv("raw_data/fern_dryweights2.csv")
drymass <- merge(drymass, niche)

#there are gonna be some redos with pv curves and sla so index drymass 
#where sladrymass !is.na
sla_mass <- drymass[!(is.na(drymass$sla_dry)),]
sla_mass$totalarea_cm2 <- with(sla_mass, (sla_no_disks * size_mm2)/100)
sla_mass$sla <- with(sla_mass, totalarea_cm2/sla_dry)



boxplot(lamina_area_cm2 ~ niche, data=area, ylim=c(0, 1750),xaxt='n',
        ylab=expression(Lamina~area~~(cm^2)), outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=1700, labels=niche_lab2)


boxplot(frond_length_cm ~ niche, data=length, ylim=c(0, 177),xaxt='n',
        ylab = "Frond length  (cm)",outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=170, labels=niche_lab2)