#LMA for different tropical fern niche habitats
source("master_scripts/plot_objects.R")

drymass <- read.csv("raw_data/fern_dryweights2.csv")

#there are gonna be some redos with pv curves and sla so index drymass 
#where sladrymass !is.na

sla_mass <- drymass[!(is.na(drymass$sla_dry)),]
  sla_mass$totalarea_cm2 <- with(sla_mass, (sla_no_disks * size_mm2)/100)
  sla_mass$sla <- with(sla_mass, totalarea_cm2/sla_dry)


# plotting ----------------------------------------------------------------

boxplot(sla ~ niche, data=sla_mass, ylab="SLA (g/cm2)", outline=FALSE)

##There are some really weird numbers when only one disk is used...so go back 
## and check data but for now lets drop them

sla_mass_clean <- sla_mass[sla_mass$sla_no_disks > 1,]

windows()
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(sla ~ niche, data=sla_mass_clean, outline=FALSE,xaxt='n',ylim=c(0,210),
        ylab=expression(Specific~leaf~area~~(g/cm^2)))
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=200, labels=niche_lab2)


