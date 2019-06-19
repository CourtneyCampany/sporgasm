source("functions_packages/basic_functions.R")
library(ape)
library(Rphylip)
library(phytools)
library(tidytree)

##phylogeny
mytree <- read.tree("phylogeny/Tree_Court_MrBAyes_newick.nwk")
mytree$tip.label <- gsub("_", " ", mytree$tip.label)
mytree$tip.label <- tolower(mytree$tip.label)

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, c("dennstaedtia dissecta"))

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
alldata$lma <- with(alldata, 1/(sla_cm2g/10000)) #g m-2

library(doBy)
traits_agg <- summaryBy(n_perc + lma ~ species  + niche2,
                        data=alldata, FUN=mean2, keep.names = TRUE)
  
#set species order to match phylogeny
traits_agg$species <- gsub("_", " ", traits_agg$species)
rownames(traits_agg) <- traits_agg$species
traits_agg<- traits_agg[mytree3$tip.label,]

#calcualte pics for 2 regression traits
pic_nitro <- pic(traits_agg$n_perc, mytree3,var.contrasts=TRUE)
pic_lma <- pic(traits_agg$lma, mytree3,var.contrasts=TRUE)

xylemstipe_pic <- lm(pic_nitro[,1] ~ pic_lma[,1]  -1)
  plot(xylemstipe_pic)
  summary(xylemstipe_pic)
  confint(xylemstipe_pic) 
  anova(xylemstipe_pic)

#correlation coefficient
cc <- sqrt(summary(xylemstipe_pic)$r.squared)


