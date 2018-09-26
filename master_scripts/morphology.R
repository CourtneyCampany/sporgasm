source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")


boxplot(lamina_area_cm2 ~ niche, data=traits, ylim=c(0, 1750),xaxt='n',
        ylab=lamina_lab, outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=1700, labels=niche_lab2)


boxplot(frond_length_cm ~ niche, data=traits, ylim=c(0, 177),xaxt='n',
        ylab = frond_lab,outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=170, labels=niche_lab2)


boxplot(stipe_length_cm ~ niche, data=traits, ylim=c(0, 177),xaxt='n',
        ylab = stipe_lab,outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=170, labels=niche_lab2)