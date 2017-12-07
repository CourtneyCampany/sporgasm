source("master_scripts/plot_objects.R")
##stomatal density across tropical fern species
m2 <- function(x) mean(x, na.rm=TRUE)

stodens <- read.csv("raw_data/stomata_density.csv")

##calculate a leaf disk mean, need to average multiple region counts first

  stodens$sto_no <- with(stodens, (sto1 + sto2 + sto3)/3)
  #scale sto_no to cm2 from FOV and mm2
  stodens$sto_cm2 <- with(stodens, (sto_no/fov_mm2 )* .01)
  stodens$sto_mm2 <- with(stodens, (sto_no/fov_mm2 ))

#lets get the mean by dish
library(doBy)

sto_agg <- summaryBy(sto_mm2 ~ niche, data=stodens, FUN=mean)
sto_agg2 <- summaryBy(sto_mm2 ~ species + niche, data=stodens, FUN=m2)

write.csv(sto_agg, "calculated_data/stomden_means.csv", row.names = FALSE)
write.csv(sto_agg2, "calculated_data/stomden_species_means.csv", row.names = FALSE)

windows()
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(sto_mm2 ~ niche, data=stodens, xaxt='n',ylim=c(0, 210),
        ylab=expression(Stomatal~densit~~(cm^2)), outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=200, labels=niche_lab2)