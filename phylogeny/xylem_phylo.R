library(picante)
source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

#xylem data
xylem <- read.csv("calculated_data/xylem_area_huber.csv")
xylem$niche2<-factor(xylem$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
xylem$id <- paste(xylem$genusspecies, xylem$plant_no, sep="-")

library(doBy)
xa_agg <- summaryBy(xylem_area_mm2 ~ species + niche2,
                       data=xylem, FUN=mean2, keep.names = TRUE)
xa_agg$sppcols <- ifelse(xa_agg$niche2 == "terrestrial", trtcols[1], "red")
xa_agg$sppcols <- ifelse(xa_agg$niche2 == "hemi-epiphyte", trtcols[2], 
                         xa_agg$sppcols)
xa_agg$sppcols <- ifelse(xa_agg$niche2 == "epiphyte", trtcols[3], 
                         xa_agg$sppcols)
xa_agg$species <- gsub("_", " ", xa_agg$species)
library(Hmisc)
xa_agg$species <- capitalize(xa_agg$species)

##phylogeny
mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
  mytree$tip.label <- gsub("_", " ", mytree$tip.label)
#need to drop the root species he added and missing xylem sample
mytree2 <- drop.tip(mytree, c("Dennstaedtia dissecta",
                              "Pecluma pectinata"))

#make sure species are in same order for data and tree
rownames(xa_agg) <- xa_agg$species
xa_agg<- xa_agg[mytree2$tip.label[1:38],]

##plot bits ------

# #add species as names of each value
xa <- xa_agg$xylem_area_mm2
names(xa)<-row.names(xa_agg)

#check if need to remove a node becase of Pec pec
node.label <- fernnode_perc
node.nopecpec <- node.label[-20]
  
library(phytools)

phycols <- c("grey85","black")
 
obj <- contMap(mytree2, xa)
obj2 <-setMap(obj,colors=phycols)
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)

xval <- lastPP$xx[1:38]
yval <- lastPP$yy[1:38]

#ancestrial state reconstruction
jpeg(filename = "manuscript/asr_xa.jpeg",
      width = 6, height = 6, units = "in", res= 400)

# windows()

plot(obj2, ftype="off", xlim=c(0,.15),ylim=c(-4,39),
     #ylim=lastPP$y.lim,xlim=lastPP$x.lim,
     outline=F,res=200, sig=2,lwd=3,
     legend=FALSE, mar=c(.4,0.4,0.4,0))

nodelabels(node.nopecpec,
           node=1:mytree2$Nnode+Ntip(mytree2),
           adj=c(1.25,-.4),
           frame="none", cex=.5)

text(xval,yval, obj2$tree$tip.label, pos=4, 
     col=xa_agg$sppcols, font=3, cex=.6)

add.color.bar(.06,obj2$cols,title=xylem_lab,
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
