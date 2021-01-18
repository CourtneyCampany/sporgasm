library(picante)
source("functions_packages/basic_functions.R")

##phylogeny
# mytree <- read.tree("phylogeny/Tree_Court_MrBayes_newick.nwk")
mytree <- read.tree("phylogeny/constrain_tree_fern.nwk")
# mytree$tip.label <- gsub("_1428_bp", "", mytree$tip.label)
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
              chl_mg_m2 +  huber + stomatal_length_um +waterpot_tlp + 
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

#traits with nona's
sd_K <- phylosignal(data_means$sd_mm2, mytree3)
fl_K <- phylosignal(data_means$frond_length_cm, mytree3)
sl_K <- phylosignal(data_means$stipe_length_cm, mytree3)
la_K <- phylosignal(data_means$lamina_area_cm2, mytree3)
chl_K <- phylosignal(data_means$chl_mg_m2, mytree3)
sla_K <- phylosignal(data_means$sla_cm2g, mytree3)
d13_K <- phylosignal(data_means$d13C, mytree3)
nitro_K <- phylosignal(data_means$n_perc, mytree3)

#will have to remove nodes to make others work,indvidually
#xylem
#huber

##pv curves all  miss diplazium_atirrense

mytree_pv <-  drop.tip(mytree3, "diplazium_atirrense")
pvdat <- data_means[complete.cases(data_means$waterpot_tlp),]

tlp_K <- phylosignal(pvdat$waterpot_tlp , mytree_pv)
op_K <- phylosignal(pvdat$osmotic_potential, mytree_pv)
ela_K <- phylosignal(pvdat$elasticity, mytree_pv)
capf_K <- phylosignal(pvdat$capacitance_full, mytree_pv)
capz_K <- phylosignal(pvdat$capacitance_zero., mytree_pv)


##stomatasize measurements all miss 
##pleopeltis_bradeorum & elaphoglossum_amygdalifolium

mytree_ss <-  drop.tip(mytree3, c("pleopeltis_bradeorum", 
                                  "elaphoglossum_amygdalifolium"))
                       
ssdat <- data_means[complete.cases(data_means$stomatal_size),] 

ss_K <- phylosignal(ssdat$stomatal_size , mytree_ss)
length_K <- phylosignal(ssdat$stomatal_length_um, mytree_ss)
width_K <- phylosignal(ssdat$average_guardcell_width_um , mytree_ss)

##xylem/huber miss pecluma_pectinata

mytree_xa <-  drop.tip(mytree3, "pecluma_pectinata")
xadat <- data_means[complete.cases(data_means$xylem_area_mm2),] 

xa_K <- phylosignal(xadat$xylem_area_mm2, mytree_xa)
hv_K <- phylosignal(xadat$huber, mytree_xa)


library(dplyr)
kstats <- bind_rows(c(sd_K, ss_K,length_K, width_K, xa_K, hv_K, tlp_K,
                      op_K, ela_K, capf_K, capz_K, fl_K, sl_K, la_K, chl_K,
                      sla_K, d13_K, nitro_K))
                       
kstats <- bind_rows(sd_K, ss_K, length_K, width_K,xa_K,hv_K, tlp_K,
                    op_K, ela_K, capf_K,capz_K, fl_K, sl_K,la_K, chl_K,
                    sla_K,d13_K,nitro_K)
kstats$variable <- c("Stomatal Density", "Stomatal Size","Guard cell length",
                     "Guard cell width", "Xylem area","Huber value", 
                     "TLP", "Osmotic Potential", "Elasticity","Capacitance full",
                     "Capacitance zero", "Frond length","Stipe length", 
                     "Lamina area", "Chlorophyll content", "SLA", "13C",
                     "Foliar Nitrogen")

write.csv(kstats, "calculated_data/K_statistic_traits_new.csv", row.names = FALSE)                     
