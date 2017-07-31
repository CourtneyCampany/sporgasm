#LMA for different tropical fern niche habitats

drymass <- read.csv("raw_data/fern_dryweights2.csv")

traits <- read.csv("raw_data/fern_traits.csv")

#merge datasets and calculate leaf mass per area
#there are gonna be some redos with pv curves and sla so index drymass where sladrymass !is.na

sla_mass <- drymass[!(is.na(drymass$sla_dry)),]

lma<- merge(sla_mass, traits)
