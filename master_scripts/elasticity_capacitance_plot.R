source("master_scripts/plot_objects.R")

pv <- read.csv("calculated_data/pv_curves2.csv")
pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
pv$niche2 <- as.factor(pv$niche2)

#reorder from ground to canopy 
pv$niche2<-factor(pv$niche2, 
                  levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)

plot(elasticity ~ capacitance_full, data=pv)
