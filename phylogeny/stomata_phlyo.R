library(picante)
source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")


#stomata
stodens <- read.csv("calculated_data/stomata_density.csv")
stodens$niche2 <- gsub("climber", "terrestrial", stodens$niche)
stodens$niche2 <- as.factor(stodens$niche2)

#reorder from ground to canopy 
stodens$niche2<-factor(stodens$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get means of stomata density per individual (3 disks total)
sd_agg <- doBy::summaryBy(sd_mm2 ~ + species + niche2,
                          data=stodens, FUN=mean, keep.names = TRUE)
sd_agg$species <- gsub("_", " ", sd_agg$species)
library(Hmisc)
sd_agg$species <- capitalize(sd_agg$species)

sd_agg$sppcols <- ifelse(sd_agg$niche2 == "terrestrial", trtcols[1], "red")
sd_agg$sppcols <- ifelse(sd_agg$niche2 == "hemi-epiphyte", trtcols[2], 
                         sd_agg$sppcols)
sd_agg$sppcols <- ifelse(sd_agg$niche2 == "epiphyte", trtcols[3], 
                         sd_agg$sppcols)


##phylogeny
# mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
mytree <- read.tree("phylogeny/constrain_tree_fern.nwk")
# mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
mytree$tip.label <- gsub("_", " ", mytree$tip.label)

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, "Dennstaedtia dissecta")

#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)

#make sure species arein same order for data and tree
rownames(sd_agg) <- sd_agg$species
sd_agg<- sd_agg[mytree3$tip.label,]

#make sure species are in same order for data and tree
rownames(sd_agg) <- sd_agg$species
sd_agg<- sd_agg[mytree3$tip.label,]

# #add species as names of each value
stom <- sd_agg$sd_mm2
names(stom)<-row.names(sd_agg)

library(phytools)
#ancestrial state reconstruction

node.label <- fernnode_perc

#plot bits
phycols <- c("grey85","black")

obj <- contMap(mytree2, stom)
obj2 <-setMap(obj,colors=phycols)
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)

xval <- lastPP$xx[1:39]
yval <- lastPP$yy[1:39]

#plot acr stomata

 jpeg(filename = "manuscript/asr_stomata.jpeg",
      width = 6, height = 6, units = "in", res= 400)

# windows()

# pdf(file = "manuscript/Figure7.pdf", width = 8, height = 8)

plot(obj2, ftype="off", xlim=c(0,.15),ylim=c(-4,39),
     #ylim=lastPP$y.lim,xlim=lastPP$x.lim,
     outline=F,res=200, sig=2,lwd=3,
     legend=FALSE, mar=c(.4,0.4,0.4,0))

nodelabels(node.label,
           node=1:mytree2$Nnode+Ntip(mytree2),
           adj=c(1.25,-.4),
           frame="none", cex=.5)

text(xval,yval, obj2$tree$tip.label, pos=4, 
     col=sd_agg$sppcols, font=3, cex=.6)

add.color.bar(.06,obj2$cols,title=sd_lab,
              lims=obj$lims,digits=1,prompt=FALSE,
              outline=FALSE,
              x=0, y=-3,
              lwd=3,fsize=.6,subtitle="")

add.color.bar(.019,"black",title="",
              lims=c(0,0.02),digits=2,
              subtitle="nucleotide substitutions per site",
              prompt=FALSE,
              outline=FALSE,
              x=.120, y=-3,
              lwd=3,fsize=.6,subtitle="")

add.simmap.legend(leg = niche_lab_goodorder,outline=FALSE,
                  colors=trtcols,prompt=FALSE,
                  x=.07,y=-1.75,vertical=TRUE,fsize=0.6)

text(0,7.8, "E2", cex=.7)
text(.0115,29.5, "E1", cex=.7)

dev.off()
