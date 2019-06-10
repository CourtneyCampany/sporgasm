library(picante)
source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

#xylem
xylem <- read.csv("calculated_data/xylem_area_huber.csv")
xylem$niche2<-factor(xylem$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
xylem2 <- xylem[xylem$xylem_area_mm2 < 0.8,]

#get means of stomata density per individual (3 disks total)
xa_agg <- doBy::summaryBy(xylem_area_mm2 ~ + species + niche2,
                          data=xylem2, FUN=mean, keep.names = TRUE)
xa_agg$species <- gsub("_", " ", xa_agg$species)
library(Hmisc)
xa_agg$species <- capitalize(xa_agg$species)

xa_agg$sppcols <- ifelse(xa_agg$niche2 == "terrestrial", trtcols[1], "red")
xa_agg$sppcols <- ifelse(xa_agg$niche2 == "hemi-epiphyte", trtcols[2], 
                         xa_agg$sppcols)
xa_agg$sppcols <- ifelse(xa_agg$niche2 == "epiphyte", trtcols[3], 
                         xa_agg$sppcols)

##phylogeny
mytree <- read.tree("phylogeny/court_phylo.nwk")
mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
mytree$tip.label <- gsub("_", " ", mytree$tip.label)

treeorder <- mytree$tip.label

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, c("Pecluma pectinata", "Dennstaedtia dissecta"))

#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)

#make sure species arein same order for data and tree
rownames(xa_agg) <- xa_agg$species
xa_agg<- xa_agg[mytree3$tip.label,]

# #add species as names of each value
xylem <- xa_agg$xylem_area_mm2
names(xylem)<-row.names(xa_agg)

library(phytools)
#ancestrial state reconstruction

# jpeg(filename = "output/asr_stipe.jpeg",
#       width = 10, height = 6, units = "in", res= 400)  

phycols <- c("grey85","grey20")

obj <- contMap(mytree2, xylem, plot=FALSE)
obj2 <-setMap(obj,colors=phycols)
lastPP<-get("last_plot.phylo",envir=.PlotPhyloEnv)


xval <- lastPP$xx[1:38]
yval <- lastPP$yy[1:38]

#plot acr stomata

# jpeg(filename = "manuscript/asr_xylem.jpeg",
#      width = 6, height = 6, units = "in", res= 400)

 windows()
plot(obj2, ftype="off", xlim=c(0,.15),ylim=c(-4,39),
     #ylim=lastPP$y.lim,xlim=lastPP$x.lim,
     outline=F,res=200, sig=2,lwd=3,
     legend=FALSE, mar=c(.4,0.4,0.4,0))

text(xval,yval, obj2$tree$tip.label, pos=4, 
     col=xa_agg$sppcols, font=3, cex=.6)

add.color.bar(.06,obj2$cols,title=xylem_lab,
              lims=obj$lims,digits=3,prompt=FALSE,
              outline=FALSE,
              x=0, y=-3,
              lwd=3,fsize=.6,subtitle="")

add.simmap.legend(leg = niche_lab_goodorder,outline=FALSE,
                  colors=trtcols,prompt=FALSE,
                  x=.07,y=-1.75,vertical=TRUE,fsize=0.6)

# 
# dev.off()
