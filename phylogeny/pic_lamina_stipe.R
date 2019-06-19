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
mytree2 <- drop.tip(mytree, "dennstaedtia dissecta")

#fix the 'not rooted' error because of zero branch lengths
is.binary(mytree2)
mytree3<-multi2di(mytree2)

#RUN PIC analysis for each continuous trait regression-------

##frond morphology data
traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
library(doBy)
traits_agg <- summaryBy(lamina_area_cm2 + stipe_length_cm ~ species + niche2,
                        data=traits, FUN=mean2, keep.names = TRUE)
  
#set species order to match phylogeny
traits_agg$species <- gsub("_", " ", traits_agg$species)
rownames(traits_agg) <- traits_agg$species
traits_agg<- traits_agg[mytree3$tip.label,]

#calcualte pics for 2 regression traits
pic_lamina <- pic(traits_agg$lamina_area_cm2, mytree3,var.contrasts=TRUE)
pic_stipe <- pic(traits_agg$stipe_length_cm, mytree3,var.contrasts=TRUE)

frondstipe_pic <- lm(pic_lamina[,1] ~ pic_stipe[,1] -1)
  plot(frondstipe_pic)
  summary(frondstipe_pic)
  confint(frondstipe_pic) 
  anova(frondstipe_pic)

#correlation coefficient
cc <- sqrt(summary(frondstipe_pic)$r.squared)



#correlation matrix ---------
vcv.phylo(mytree2, cor=TRUE)

library(nlme)
frond_gls <- gls(frond_length_cm ~ niche2, data=traits_agg, 
                 correlation=corBrownian(1,mytree2))
plot(frond_gls)
summary(frond_gls) #terrestrial different from epiphyte
confint(frond_gls) 
anova(frond_gls)

frond_gls1 <- gls(frond_length_cm ~ niche2, data=traits_agg, 
                 correlation=corPagel(1,mytree2, fixed=TRUE))
frond_gls0 <- gls(frond_length_cm ~ niche2, data=traits_agg, 
                 correlation=corPagel(0,mytree2, fixed=TRUE))
frond_gls.5 <- gls(frond_length_cm ~ niche2, data=traits_agg, 
                 correlation=corPagel(.5,mytree2, fixed=TRUE))

maxlike_laambda <- gls(frond_length_cm ~ niche2, data=traits_agg,
         correlation=corPagel(1, mytree2, fixed = FALSE))


orn_uhl <- gls(frond_length_cm ~ niche2, data=traits_agg,
         correlation=corMartins(1,mytree2,fixed=FALSE)) #ornstein process



#tranfomr like traditional lmer???
frond_gls2 <- gls(sqrt(frond_length_cm) ~ niche2, data=traits_agg, 
                 correlation=corBrownian(1,mytree2))
plot(frond_gls2)
