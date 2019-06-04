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
mytree2 <- drop.tip(mytree, "dennstaedtia_dissecta")

#fix the 'not rooted' error because of zero branch lengths
is.binary(mytree2)
mytree3<-multi2di(mytree2)


##frond morphology data
traits <- read.csv("calculated_data/fern_traits.csv")
  traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
library(doBy)
traits_agg <- summaryBy(frond_length_cm + stipe_length_cm ~ species + niche2,
                        data=traits, FUN=mean2, keep.names = TRUE)
  
traits_agg$species<-factor(traits_agg$species, levels=treeorder)
rownames(traits_agg) <- traits_agg$species


pic_frond <- pic(traits_agg$frond_length_cm, mytree3)
pic_niche2 <- pic(traits_agg$niche2, mytree3)

frond_pic_lm <- lm(pic_frond ~ pic_niche2 -1)
  plot(frond_pic_lm)
  summary(frond_pic_lm)
  confint(frond_pic_lm) 
  anova(frond_pic_lm)

#correlation coefficient
cc <- sqrt(summary(frond_pic_lm)$r.squared)


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
