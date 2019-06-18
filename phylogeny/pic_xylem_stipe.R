source("functions_packages/basic_functions.R")
library(ape)
library(Rphylip)
library(phytools)
library(tidytree)

##phylogeny
mytree <- read.tree("phylogeny/court_phylo.nwk")
mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
mytree$tip.label <- tolower(mytree$tip.label)

treeorder <- mytree$tip.label

#need to drop the extra species he added
mytree2 <- drop.tip(mytree, c("dennstaedtia_dissecta", "pecluma_pectinata"))

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
alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]
alldata3 <- alldata2[complete.cases(alldata2$xylem_area_mm2) & 
                       complete.cases(alldata2$species),]
alldata3$stipe_nozero <- alldata3$stipe_length_cm + .1
alldata4 <- alldata3[! alldata3$id  %in% c("lomjap-4","lomjap-3","lomjap-6"),]
  
library(doBy)
traits_agg <- summaryBy(xylem_area_mm2 + stipe_length_cm + stipe_nozero ~ species 
                        + niche2,
                        data=alldata4, FUN=mean2, keep.names = TRUE)

#drop missing species
  
#set species order to match phylogeny
rownames(traits_agg) <- traits_agg$species
traits_agg<- traits_agg[mytree3$tip.label,]

#calcualte pics for 2 regression traits
pic_xylem <- pic(traits_agg$xylem_area_mm2, mytree3,var.contrasts=TRUE)
pic_stipe <- pic(traits_agg$stipe_length_cm, mytree3,var.contrasts=TRUE)

xylemstipe_pic <- lm(pic_stipe[,1] ~ pic_xylem[,1]  -1) #through origin
  plot(xylemstipe_pic)
  summary(xylemstipe_pic)
  confint(xylemstipe_pic) 
  anova(xylemstipe_pic)

#correlation coefficient
cc <- sqrt(summary(xylemstipe_pic)$r.squared)


