##stomatal density across tropical fern species

stodens <- read.csv("raw_data/stomata_density.csv")

##calculate a leaf disk mean, need to average multiple region counts first

stodens$sto_no <- with(stodens, (sto1 + sto2 + sto3)/3)

#scale sto_no to cm2 from FOV and mm2
stodens$sto_cm2 <- with(stodens, (sto_no/fov_mm2 )* .01)
stodens$sto_mm2 <- with(stodens, (sto_no/fov_mm2 ))

#lets get the mean by dish
library(doBy)

sto_agg <- summaryBy(sto_mm2 ~ niche, data=stodens, FUN=mean)

write.csv(sto_agg, "caculated_data/stomden_means.csv", row.names = FALSE)

windows()
boxplot(sto_mm2 ~ niche, data=stodens, ylab="Stomatal density (cm2)", outline=FALSE)
