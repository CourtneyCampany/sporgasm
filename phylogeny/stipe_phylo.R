library(picante)
source("functions_packages/basic_functions.R")


##frond morphology data
traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- traits$niche
traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
traits$niche2 <- as.factor(traits$niche2)
#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

library(doBy)
stipe_agg <- summaryBy(stipe_length_cm ~ species + niche2,
                        data=traits, FUN=mean2, keep.names = TRUE)

##phylogeny
mytree <- read.tree("phylogeny/court_phylo.nwk")
  mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
  mytree$tip.label <- tolower(mytree$tip.label)

treeorder <- mytree$tip.label

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, "dennstaedtia_dissecta")

#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)

#make sure species arein same order for data and tree
rownames(stipe_agg) <- stipe_agg$species
stipe_agg<- stipe_agg[mytree3$tip.label,]

# stipe_K <- Kcalc(stipe_agg$stipe_length_cm, mytree3)
# stipe_K2 <- phylosignal(stipe_agg$stipe_length_cm, mytree3)


#plots
# windows()
# plot(mytree3, show.tip.label = FALSE)
# tiplabels(pch = 22, col = stipe_agg[, 3] + 1, 
#           bg = stipe_agg[, 3] + 1, cex = 1.5)
# 
# #add species as names of each value
stipe <- stipe_agg$stipe_length_cm
names(stipe)<-row.names(stipe_agg)
  
library(phytools)
# acr_sd <- fastAnc(mytree3, stipe, CI=TRUE)
# 
# windows()
# plot(mytree3, label.offset=.1)
# tiplabels(round(stipe,3), adj=c(-0.5,0.5), cex=0.7)
# nodelabels(round(acr_sd[[1]],3), adj=c(-0.1,0.5), cex=0.6, frame="n")

#ancestrial state reconstruction

# jpeg(filename = "output/asr_stipe.jpeg",
#       width = 10, height = 6, units = "in", res= 400)  

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
contMap(mytree2, stipe)
# 
# dev.off()
