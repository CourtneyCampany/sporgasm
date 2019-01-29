source("master_scripts/plot_objects.R")

traits <- read.csv("calculated_data/fern_traits.csv")

windows()

par(mar=c(5,5,1,1), mfrow=c(3,1))

# first plots------
boxplot(lamina_area_cm2 ~ niche, data=traits, ylim=c(0, 1750),xaxt='n',
        ylab=lamina_lab, outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=1700, labels=niche_lab2)


boxplot(frond_length_cm ~ niche, data=traits, ylim=c(0, 177),xaxt='n',
        ylab = frond_lab,outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)
text(x=c(1,2,3,4),y=170, labels=niche_lab2)


boxplot(stipe_length_cm ~ niche, data=traits, ylim=c(0, 80),xaxt='n',
        ylab = stipe_lab,outline=FALSE)
axis(1, niche_lab, at=1:4, cex=1.1)

text(x=c(1,2,3,4),y=75, labels=niche_lab2)


## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  
  no_terr <- length(unique(traits[traits$niche2 =="terrestrial", "species"]))
  no_epi <- length(unique(traits[traits$niche =="epiphyte", "species"]))
  no_hemi <- length(unique(traits[traits$niche =="hemi-epiphyte", "species"]))
  
niche_count2 <- c(no_terr, no_hemi, no_epi)
  niche_lab <- paste("n=", niche_count2, sep = "")
  
#boxplots-----  
  gradient <- colorRampPalette(c("forestgreen","steelblue4"))
  palette(gradient(3))
  trtcols <- palette(gradient(3))
  
#reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
               levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
boxlabs <- c("Terrestrial", "Hemi-epiphyte", "Epiphyte")
  

jpeg(filename = "jobtalk/leafsize.jpeg",
     width = 7, height = 7, units = "in", res= 400)

par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))

boxplot(lamina_area_cm2 ~ niche2, data=traits, ylim=c(0, 2000),xaxt='n',
          ylab=lamina_lab, outline=FALSE, col=trtcols)
  axis(1, boxlabs, at=1:3, cex=1.1,cex.axis=1.25)
  text(x=c(1,2,3),y=1900, labels=niche_lab)

dev.off()
  
boxplot(frond_length_cm ~ niche2, data=traits, ylim=c(0, 200),xaxt='n',
          ylab = frond_lab,outline=FALSE)
  axis(1, niche_lab_noclimb, at=1:3, cex=1.1)
  text(x=c(1,2,3,4),y=190, labels=niche_lab3)
  

jpeg(filename = "jobtalk/stipe.jpeg",
       width = 7, height = 7, units = "in", res= 400)  

par(las=1, cex.axis=1, cex.lab=1.25, mgp=c(3,1,0), mar=c(4,5,1,1))
boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 75),xaxt='n',
          ylab = stipe_lab,outline=FALSE, col=trtcols)
axis(1, boxlabs, at=1:3, cex=1.1,cex.axis=1.25)
text(x=c(1,2,3),y=70, labels=niche_lab)
dev.off()
