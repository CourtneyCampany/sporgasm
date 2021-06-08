
### analyze vulnerability curves for each species

library(dplyr)

#read in all vcurves (working) -----------

laselva <- read.csv("calculated_data/laselva_vcurves.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="la_selva")

lascruces <- read.csv("calculated_data/lascruces_vcurves2.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="las_cruces")

#merge sites
fern_plc <- merge(laselva, lascruces, all=TRUE)
# add species and habitat info
species_info <- read.csv("raw_data/species_niches.csv")

fern_plc_habitat <- merge(fern_plc, species_info, by=c("genusspecies", "site"))
#have only one climber to reclassify as terrestrial
fern_plc_habitat$niche2 <- fern_plc_habitat$niche 
fern_plc_habitat$niche2 <- gsub("climber", "hemi-epiphyte", 
                                fern_plc_habitat$niche2) %>% as.factor()

#reorder from ground to canopy 
fern_plc_habitat$niche2<-factor(fern_plc_habitat$niche2, 
                                levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))



## there are a few species that only have Kmax, lets remove them     
fern_plc_curves <- fern_plc_habitat[!fern_plc_habitat$genusspecies 
                                    %in% c("aspuni", "eladec","nipcra") & 
                                      !fern_plc_habitat$curve_id 
                                    %in% c("lommax-2", "lommax-5", "lommax-6","lommax-7"),]

##load fitplc (can use remko's dev version or updated CRAN package)

# latest from bitbucket: important changes will be pushed to CRAN soon
if(FALSE){
  remotes::install_bitbucket("remkoduursma/fitplc")
}
library(fitplc)
if(packageVersion("fitplc") < "1.3"){
  stop("did I not tell you to update?")
}

#### calculate Px parameters from each curve: -----------

#####RERUN with or without rescale (sigmodial or no)


# As there are a reasonable # of species (14), run fitplcs by species
# that way I can inspect each curve and look for bad data

species <- unique(fern_plc_curves$genusspecies)

library(plyr)

#create easy object for package required variables names
var_names <- c(PLC = "PLC", WP = "MPa")

# #1) cycsem ----

cycsem <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "cycsem",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
cycsem_Px_12 <- getPx(cycsem,  rescale_Px=TRUE, x=12)
cycsem_Px_50 <- getPx(cycsem,  rescale_Px=TRUE, x=50)
cycsem_Px_88 <- getPx(cycsem,  rescale_Px=TRUE, x=88)
cycsem_Px <- rbind.fill(cycsem_Px_12,cycsem_Px_50,cycsem_Px_88)


#2) dippro

dippro <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "dippro",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
dippro_Px_12 <- getPx(dippro,  rescale_Px=TRUE, x=12)
dippro_Px_50 <- getPx(dippro,  rescale_Px=TRUE, x=50)
dippro_Px_88 <- getPx(dippro,  rescale_Px=TRUE, x=88)
dippro_Px <- rbind.fill(dippro_Px_12,dippro_Px_50,dippro_Px_88)

#3) drypat

drypat <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "drypat",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
#singulargradient

drypat_no2 <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "drypat"&
                                        ! fern_plc_curves$curve_id == "drypat-2",],"curve_id",
                      model = "Weibull", rescale_Px=TRUE, varnames=var_names)

drypat_Px_12 <- getPx(drypat_no2,  rescale_Px=TRUE, x=12)
drypat_Px_50 <- getPx(drypat_no2,  rescale_Px=TRUE, x=50)
drypat_Px_88 <- getPx(drypat_no2,  rescale_Px=TRUE, x=88)
drypat_Px <- rbind.fill(drypat_Px_12,drypat_Px_50,drypat_Px_88)

#4) elalat

elalat <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "elalat",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
elalat_Px_12 <- getPx(elalat,  rescale_Px=TRUE, x=12)
elalat_Px_50 <- getPx(elalat,  rescale_Px=TRUE, x=50)
elalat_Px_88 <- getPx(elalat,  rescale_Px=TRUE, x=88)
elalat_Px <- rbind.fill(elalat_Px_12,elalat_Px_50,elalat_Px_88)

#5 lomjap

lomjap <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "lomjap",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
lomjap_Px_12 <- getPx(lomjap,  rescale_Px=TRUE, x=12)
lomjap_Px_50 <- getPx(lomjap,  rescale_Px=TRUE, x=50)
lomjap_Px_88 <- getPx(lomjap,  rescale_Px=TRUE, x=88)
lomjap_Px <- rbind.fill(lomjap_Px_12,lomjap_Px_50,lomjap_Px_88)

#6) lommax

lommax <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "lommax",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
lommax_Px_12 <- getPx(lommax,  rescale_Px=TRUE, x=12)
lommax_Px_50 <- getPx(lommax,  rescale_Px=TRUE, x=50)
lommax_Px_88 <- getPx(lommax,  rescale_Px=TRUE, x=88)
lommax_Px <- rbind.fill(lommax_Px_12,lommax_Px_50,lommax_Px_88)

#7) olecos

olecos <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "olecos",],
                  "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
olecos_Px_12 <- getPx(olecos,  rescale_Px=TRUE, x=12)
olecos_Px_50 <- getPx(olecos,  rescale_Px=TRUE, x=50)
olecos_Px_88 <- getPx(olecos,  rescale_Px=TRUE, x=88)
olecos_Px <- rbind.fill(olecos_Px_12,olecos_Px_50,olecos_Px_88)

#8) phlaur

phlaur <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "phlaur",],
                  "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
phlaur_Px_12 <- getPx(phlaur,  rescale_Px=TRUE, x=12)
phlaur_Px_50 <- getPx(phlaur,  rescale_Px=TRUE, x=50)
phlaur_Px_88 <- getPx(phlaur,  rescale_Px=TRUE, x=88)
phlaur_Px <- rbind.fill(phlaur_Px_12,phlaur_Px_50,phlaur_Px_88)

#9) polosm
polosm <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "polosm",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
polosm_Px_12 <- getPx(polosm,  rescale_Px=TRUE, x=12)
polosm_Px_50 <- getPx(polosm,  rescale_Px=TRUE, x=50)
polosm_Px_88 <- getPx(polosm,  rescale_Px=TRUE, x=88)
polosm_Px <- rbind.fill(polosm_Px_12,polosm_Px_50,polosm_Px_88)

#10) serfra
serfra <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "serfra",], 
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
serfra_Px_12 <- getPx(serfra,  rescale_Px=TRUE, x=12)
serfra_Px_50 <- getPx(serfra,  rescale_Px=TRUE, x=50)
serfra_Px_88 <- getPx(serfra,  rescale_Px=TRUE, x=88)
serfra_Px <- rbind.fill(serfra_Px_12,serfra_Px_50,serfra_Px_88)

#11) sertri
sertri <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "sertri",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
sertri_Px_12 <- getPx(sertri,  rescale_Px=TRUE, x=12)
sertri_Px_50 <- getPx(sertri,  rescale_Px=TRUE, x=50)
sertri_Px_88 <- getPx(sertri,  rescale_Px=TRUE, x=88)
sertri_Px <- rbind.fill(sertri_Px_12,sertri_Px_50,sertri_Px_88)

#12) tecinc

tecinc <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "tecinc",], 
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
tecinc_Px_12 <- getPx(tecinc,  rescale_Px=TRUE, x=12)
tecinc_Px_50 <- getPx(tecinc,  rescale_Px=TRUE, x=50)
tecinc_Px_88 <- getPx(tecinc,  rescale_Px=TRUE, x=88)
tecinc_Px <- rbind.fill(tecinc_Px_12,tecinc_Px_50,tecinc_Px_88)


#13) olfcer

olfcer <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "olfcer",],
          "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)
olfcer_Px_12 <- getPx(olfcer,  rescale_Px=TRUE, x=12)
olfcer_Px_50 <- getPx(olfcer,  rescale_Px=TRUE, x=50)
olfcer_Px_88 <- getPx(olfcer,  rescale_Px=TRUE, x=88)
olfcer_Px <- rbind.fill(olfcer_Px_12,olfcer_Px_50,olfcer_Px_88)
#singular gradient error in nls
# olfcer_Px <- getPx(olfcer,  rescale_Px=TRUE, x=c(12, 50, 88))

#run indivduals and adjust fitting methods as needed
# olfcer_2 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-2",], 
#                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)
# 
# olfcer_3 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-3",], 
#                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)
# 
# olfcer_4 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-4",], 
#                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)
# 
# olfcer_5 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-5",], 
#                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)
# 
# #droppping curve six, 
# # olfcer_6 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-6",], 
# #                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)
# 
# olfcer_7 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "olfcer-7",], 
#                    model = "Weibull", rescale_Px=TRUE, varnames=var_names)

# t2 <- getPx(olfcer_2, rescale_Px=TRUE, x=c(12, 50, 88))
#     t2$Group <- "olfcer-2"
# t3 <- getPx(olfcer_3, rescale_Px=TRUE, x=c(12, 50, 88))
#     t3$Group <- "olfcer-3"
# t4 <- getPx(olfcer_4, rescale_Px=TRUE, x=c(12, 50, 88))
#     t4$Group <- "olfcer-4"
# t5 <- getPx(olfcer_5, rescale_Px=TRUE, x=c(12, 50, 88))
#     t5$Group <- "olfcer-5"
# t7 <- getPx(olfcer_7, rescale_Px=TRUE, x=c(12, 50, 88))
#     t7$Group <- "olfcer-7"
# 
# olfcer_Px <- dplyr::bind_rows(t2,t3,t4,t5,t7)
# olfcer_Px <- olfcer_Px[,c(5, 1:4)]


#14) parexc

parexc_no1 <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "parexc"&
              ! fern_plc_curves$curve_id == "parexc-1",],"curve_id",
              model = "Weibull", rescale_Px=TRUE, varnames=var_names)

# parexc <- fitplcs(fern_plc_curves[fern_plc_curves$genusspecies == "parexc",],
#                   "curve_id",model = "Weibull", rescale_Px=TRUE, varnames=var_names)

#dropping parexc1
# parexc_1 <- fitplc(fern_plc_curves[fern_plc_curves$curve_id == "parexc-1",],
#                    model = "loess", rescale_Px=TRUE, varnames=var_names)

parexc_Px_12 <- getPx(parexc_no1,  rescale_Px=TRUE, x=12)
parexc_Px_50 <- getPx(parexc_no1,  rescale_Px=TRUE, x=50)
parexc_Px_88 <- getPx(parexc_no1,  rescale_Px=TRUE, x=88)
parexc_Px <- rbind.fill(parexc_Px_12,parexc_Px_50,parexc_Px_88)


#merge all parameters and species
species_vc_px <- dplyr::bind_rows(cycsem_Px, dippro_Px, drypat_Px,
                                elalat_Px,lomjap_Px,lommax_Px,olecos_Px,
                                olfcer_Px,parexc_Px,phlaur_Px,polosm_Px,
                                sertri_Px, serfra_Px, tecinc_Px)

write.csv(species_vc_px, "calculated_data/species_p50_new.csv", row.names = FALSE)


#examine fits
par(mfrow=c(2,3))
plot(lomjap)


