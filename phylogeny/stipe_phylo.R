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

stipe_agg$species<-factor(traits_agg$species, levels=treeorder)
rownames(traits_agg) <- traits_agg$species


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
stipe_agg$species<-factor(stipe_agg$species, levels=treeorder)
rownames(stipe_agg) <- traits_agg$species

stipe_K <- Kcalc(stipe_agg$stipe_length_cm, mytree3)
stipe_K2 <- phylosignal(stipe_agg$stipe_length_cm, mytree3)


#plots
windows()
plot(mytree3, show.tip.label = FALSE)
tiplabels(pch = 22, col = stipe_agg[, 3] + 1, 
          bg = stipe_agg[, 3] + 1, cex = 1.5)
