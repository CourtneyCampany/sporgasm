
library(dplyr)

#read in all vcurves

laselva <- read.csv("calculated_data/laselva_vcurves.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="la_selva")

lascruces <- read.csv("calculated_data/lascruces_vcurves.csv") %>%
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
# if(FALSE){
#   remotes::install_bitbucket("remkoduursma/fitplc")
# }
library(fitplc)
# if(packageVersion("fitplc") < "1.3"){
#   stop("did I not tell you to update?")
# }



### analyze vulnerability curves for each species

# quick plot check of each curve
library(ggplot2)
windows()
ggplot(fern_plc_curves, aes(MPa,PLC)) +
  geom_line() +
  facet_wrap(~curve_id) +
  theme_minimal()


library(dplyr)

#read in all vcurves (working) -----------

laselva <- read.csv("calculated_data/laselva_vcurves.csv") %>%
  mutate(curve_id = paste(genusspecies,individual,sep="-"),
         site="la_selva")

lascruces <- read.csv("calculated_data/lascruces_vcurves.csv") %>%
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

#create easy object for package required variables names
var_names <- c(PLC = "PLC", WP = "MPa")


#1) cycsem ----

cycsem <- fitplc(fern_plc_curves[fern_plc_curves$genusspecies == "cycsem",],
          random=individual,model = "Weibull", varnames=var_names)

cycsem_Px <- getPx(cycsem,  x=c(12, 50, 88))
cyclab <- c(paste(12,round(cycsem_Px[1,2],2), sep="="),
            paste(50,round(cycsem_Px[2,2],2), sep="="),
            paste(88,round(cycsem_Px[3,2],2), sep="="))
 
# jpeg(filename = "output/PLC_sycsem.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(5,5,1,1), cex.lab=1)
plot(cycsem, plotrandom = TRUE, what="PLC")
text(3.45, 25, "Cyclopeltis semicordata")
legend(x=3.1, y=20, legend=cyclab, title="Px  (-MPa)", bty='n', title.adj =1 )

# dev.off()
