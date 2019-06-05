library(picante)
source("functions_packages/basic_functions.R")


##phylogeny
mytree <- read.tree("phylogeny/court_phylo.nwk")
  mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
  mytree$tip.label <- tolower(mytree$tip.label)
  
#need to drop the extra species he added
 mytree2 <- drop.tip(mytree, "dennstaedtia_dissecta")
 treeorder <- mytree2$tip.label
 
#fix the 'not rooted' error because of zero branch lengths
mytree3<-multi2di(mytree2)
  
## all traits
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#get species means
data_means <- doBy::summaryBy(stomatal_size + sd_mm2 + xylem_area_mm2 +
              frond_length_cm + stipe_length_cm + lamina_area_cm2 +
              chl_mg_m2 +  huber + guardcell_length_um +waterpot_tlp + 
              average_guardcell_width_um + osmotic_potential + elasticity +
              capacitance_full + capacitance_zero. + sla_cm2g + d13C + n_perc
              ~ species,data=alldata,FUN=mean2, keep.names = TRUE)

#make sure species arein same order for data and tree
rownames(data_means) <- data_means$species
data_means<- data_means[mytree3$tip.label,]

##some traits have NA's....
#stomatal_size
#xylem_area_mm2 
#huber
#guard_cell_width_um
##all pV traits

#stomata density
sd_K <- phylosignal(data_means$sd_mm2, mytree3)
fl_K <- phylosignal(data_means$frond_length_cm, mytree3)
sl_K <- phylosignal(data_means$stipe_length_cm, mytree3)
la_K <- phylosignal(data_means$lamina_area_cm2, mytree3)
chl_K <- phylosignal(data_means$chl_mg_m2, mytree3)
sla_K <- phylosignal(data_means$sla_cm2g, mytree3)
d13_K <- phylosignal(data_means$d13C, mytree3)
nitro_K <- phylosignal(data_means$n_perc, mytree3)

