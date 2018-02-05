##analyze and plot lamina area and lamina length
source("master_scripts/plot_objects.R")
pinnae <- read.csv("raw_data/fern_traits.csv")
niche <- read.csv("raw_data/species_niches.csv")

pinnae <- merge(pinnae, niche)

  # table(pinnae$lamina_area_cm2)
  # which(is.na(pinnae$lamina_area_cm2))
  # which(is.na(pinnae$frond_length_cm))

area <- pinnae[, c(1:4, 8, 11)]
length <- pinnae[complete.cases(pinnae), c(1:7, 11)]

# plotting ----------------------------------------------------------------

windows()
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(lamina_area_cm2 ~ niche, data=area, ylim=c(0, 1750),xaxt='n',
        ylab=expression(Lamina~area~~(cm^2)), outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=1700, labels=niche_lab2)

windows()
par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(frond_length_cm ~ niche, data=length, ylim=c(0, 177),xaxt='n',
        ylab = "Frond length  (cm)")
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=170, labels=niche_lab2)



##nice pattern in frond length when you exclude the climber species
# windows()
# boxplot(laminalength_cm ~ niche, data=length,
#         ylab = "Frond length  (cm)")
# 
# windows()
# boxplot(stipe_length_cm ~ niche, data=length, ylim=c(0, 75),
#         ylab = "Frond length  (cm)")
