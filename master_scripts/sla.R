#drymass
drymass <- read.csv("raw_data/fern_dryweights2.csv")
drymass <- merge(drymass, niche)

#there are gonna be some redos with pv curves and sla so index drymass 
#where sladrymass !is.na
sla_mass <- drymass[!(is.na(drymass$sla_dry)),]
sla_mass$totalarea_cm2 <- with(sla_mass, (sla_no_disks * size_mm2)/100)
sla_mass$sla <- with(sla_mass, totalarea_cm2/sla_dry)