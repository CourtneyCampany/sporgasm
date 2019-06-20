library(picante)
source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

#lma data
sla <- read.csv("calculated_data/fern_sla.csv")
sla$niche2<-factor(sla$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
sla$lma <- with(sla, 1/(sla_cm2g/10000)) #g m-2

library(doBy)
lma_agg <- summaryBy(lma ~ species + niche2,
                       data=sla, FUN=mean2, keep.names = TRUE)
lma_agg$sppcols <- ifelse(lma_agg$niche2 == "terrestrial", trtcols[1], "red")
lma_agg$sppcols <- ifelse(lma_agg$niche2 == "hemi-epiphyte", trtcols[2], 
                          lma_agg$sppcols)
lma_agg$sppcols <- ifelse(lma_agg$niche2 == "epiphyte", trtcols[3], 
                            lma_agg$sppcols)
lma_agg$species <- gsub("_", " ", lma_agg$species)
library(Hmisc)
lma_agg$species <- capitalize(lma_agg$species)

##phylogeny
mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
# test <- read.nexus("phylogeny/Tree_Court_MrBayes_nexus.nxs")

mytree$tip.label <- gsub("_", " ", mytree$tip.label)
#need to drop the root species he added
mytree2 <- drop.tip(mytree, "Dennstaedtia dissecta")
#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)

# out <- c("Christella dentata","Goniopteris curta", "Goniopteris nicaraguensis")
# rooted_tree <- root(mytree2, out, resolve.root=TRUE)

#make sure species are in same order for data and tree
rownames(lma_agg) <- lma_agg$species
lma_agg<- lma_agg[mytree$tip.label[1:39],]

##plot bits ------

# #add species as names of each value
lma <- lma_agg$lma
names(lma)<-row.names(lma_agg)

node.label <- fernnode_perc
  
library(phytools)

phycols <- c("grey85","black")
 
obj <- contMap(mytree2, lma)
obj2 <-setMap(obj,colors=phycols)
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)

xval <- lastPP$xx[1:39]
yval <- lastPP$yy[1:39]

#ancestrial state reconstruction
jpeg(filename = "manuscript/asr_lma.jpeg",
      width = 6, height = 6, units = "in", res= 400)
# windows()
plot(obj2, ftype="off", xlim=c(0,.15),ylim=c(-4,39),
     #ylim=lastPP$y.lim,xlim=lastPP$x.lim,
     outline=F,res=200, sig=2,lwd=3,
     legend=FALSE, mar=c(.4,0.4,0.4,0))

nodelabels(node.label,
           node=1:mytree2$Nnode+Ntip(mytree2),
           adj=c(1.25,-.4),
           frame="none", cex=.5)

text(xval,yval, obj2$tree$tip.label, pos=4, 
     col=lma_agg$sppcols, font=3, cex=.6)

add.color.bar(.06,obj2$cols,title=lma_lab,
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

dev.off()
