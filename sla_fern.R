#LMA for different tropical fern niche habitats

drymass <- read.csv("raw_data/fern_dryweights2.csv")

#merge datasets and calculate leaf mass per area
#there are gonna be some redos with pv curves and sla so index drymass where sladrymass !is.na

sla_mass <- drymass[!(is.na(drymass$sla_dry)),]
  sla_mass$totalarea_cm2 <- with(sla_mass, (sla_no_disks * size_mm2)/100)

sla_mass$sla <- with(sla_mass, totalarea_cm2/sla_dry)


boxplot(sla ~ niche, data=sla_mass, ylab="SLA (g/cm2)", outline=FALSE)

##There are some really weird numbers when only one disk is used...so go back 
## and chekc data but for now lets drop them

sla_mass_clean <- sla_mass[sla_mass$sla_no_disks > 1,]

boxplot(sla ~ niche, data=sla_mass_clean, ylab="SLA (g/cm2)", outline=FALSE)

