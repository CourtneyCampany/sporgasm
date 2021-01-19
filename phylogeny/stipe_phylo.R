library(picante)
source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")


##frond morphology data
traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

library(doBy)
stipe_agg <- summaryBy(stipe_length_cm ~ species + niche2,
                        data=traits, FUN=mean2, keep.names = TRUE)
stipe_agg$sppcols <- ifelse(stipe_agg$niche2 == "terrestrial", trtcols[1], "red")
stipe_agg$sppcols <- ifelse(stipe_agg$niche2 == "hemi-epiphyte", trtcols[2], 
                            stipe_agg$sppcols)
stipe_agg$sppcols <- ifelse(stipe_agg$niche2 == "epiphyte", trtcols[3], 
                            stipe_agg$sppcols)
stipe_agg$species <- gsub("_", " ", stipe_agg$species)
library(Hmisc)
stipe_agg$species <- capitalize(stipe_agg$species)

##phylogeny
# mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
mytree <- read.tree("phylogeny/constrain_tree_fern.nwk")

# mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
  mytree$tip.label <- gsub("_", " ", mytree$tip.label)

treeorder <- mytree$tip.label

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, "Dennstaedtia dissecta")

#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)

#make sure species are in same order for data and tree
rownames(stipe_agg) <- stipe_agg$species
stipe_agg<- stipe_agg[mytree3$tip.label,]

##plot bits ------

# #add species as names of each value
stipe <- stipe_agg$stipe_length_cm
names(stipe)<-row.names(stipe_agg)
  
library(phytools)

phycols <- c("grey85","black")

node.label <- fernnode_perc
 
obj <- contMap(mytree2, stipe)
obj2 <-setMap(obj,colors=phycols)
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)

xval <- lastPP$xx[1:39]
yval <- lastPP$yy[1:39]

#ancestrial state reconstruction
# jpeg(filename = "manuscript/asr_stipe.jpeg",
#        width = 6, height = 6, units = "in", res= 400)

 # windows()

pdf(file = "manuscript/Figure6.pdf", width = 8, height = 8)

plot(obj2, ftype="off", xlim=c(0,.15),ylim=c(-4,39),
     #ylim=lastPP$y.lim,xlim=lastPP$x.lim,
     outline=F,res=200, sig=2,lwd=3,
     legend=FALSE, mar=c(.4,0.4,0.4,0))

nodelabels(node.label,
           node=1:mytree2$Nnode+Ntip(mytree2),
           adj=c(1.25,-.4),
           frame="none", cex=.5)

text(xval,yval, obj2$tree$tip.label, pos=4, 
     col=stipe_agg$sppcols, font=3, cex=.6)

add.color.bar(.06,obj2$cols,title="Stipe length  (cm)",
              lims=obj$lims,digits=3,prompt=FALSE,
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
