source("functions_packages/basic_functions.R")
library(ape)
library(Rphylip)
library(phytools)
library(tidytree)

##phylogeny
# mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
mytree <- read.tree("phylogeny/constrain_tree_fern.nwk")
  mytree$tip.label <- gsub("_", " ", mytree$tip.label)
  mytree$tip.label <- tolower(mytree$tip.label)

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, c("dennstaedtia dissecta","pleopeltis bradeorum",
                              "elaphoglossum amygdalifolium"))

#fix the 'not rooted' error because of zero branch lengths
is.binary(mytree2)
mytree3<-multi2di(mytree2)

#RUN PIC analysis for each continuous trait regression-------

##frond morphology data
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$id <- paste(alldata$genusspecies, alldata$plant_no, sep="-")

library(doBy)
traits_agg <- summaryBy(sd_mm2 + stomatal_size ~ species  + niche2,
                        data=alldata, FUN=mean2, keep.names = TRUE)
  
#set species order to match phylogeny
traits_agg$species <- gsub("_", " ", traits_agg$species)
rownames(traits_agg) <- traits_agg$species

stomata_data <- traits_agg[complete.cases(traits_agg$stomatal_size),]
traits_agg<- traits_agg[mytree3$tip.label,]

#calcualte pics for 2 regression traits
pic_sd <- pic(traits_agg$sd_mm2, mytree3,var.contrasts=TRUE)
pic_ss <- pic(traits_agg$stomatal_size, mytree3,var.contrasts=TRUE)

stomata_pic <- lm(pic_sd[,1] ~ pic_ss[,1] -1)
  plot(stomata_pic)
  summary(stomata_pic)
  confint(stomata_pic) 
  anova(stomata_pic)

#correlation coefficient
cc <- sqrt(summary(stomata_pic)$r.squared)


