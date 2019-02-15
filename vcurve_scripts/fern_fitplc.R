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
fern_plc_habitat$niche2 <- gsub("climber", "terrestrial", 
                                fern_plc_habitat$niche2) %>% 
  as.factor()

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


# quick plot check of each curve
library(ggplot2)
windows()
ggplot(fern_plc_curves, aes(MPa,PLC)) +
  geom_line() +
  facet_wrap(~curve_id) +
  theme_minimal()


#create easy object for package required variables names
var_names <- c(PLC = "PLC", WP = "MPa")

# gloabl fits by habitats
epi_species <- droplevels(fern_plc_curves[fern_plc_curves$niche2 == "epiphyte",])
terr_species <- droplevels(fern_plc_curves[fern_plc_curves$niche2 == "terrestrial",])
hemi_species <- droplevels(fern_plc_curves[fern_plc_curves$niche2 == "hemi-epiphyte",])

#1. epiphyte
epiphyte <- fitplc(epi_species, rescale_Px=TRUE, model="loess",rescale_Px=TRUE,
                    varnames=var_names)
plot(epiphyte)
plot(epiphyte, what="embol")
epi_p50 <- getPx(epiphyte)

#2. terrestrial
terrestrial <- fitplc(terr_species, 
                       model = "loess", 
                       rescale_Px=TRUE, 
                       varnames=var_names)
plot(terrestrial, what="embol")
terr_p50 <- getPx(terrestrial)


#3. hemiepiphyte
hemi_epi <- fitplc(hemi_species, 
                    model = "loess", 
                    rescale_Px=TRUE, 
                    varnames=var_names)
plot(hemi_epi, what='embol')
hemi_p50 <- getPx(hemi_epi)


## global plotting by habitat-------------
# see ?plot.fitplc for options
PLC_lab <- "Percent loss conductivity  (%)"
p50_lab <- expression(P[50])

#habitat colors
gradient <- colorRampPalette(c("orange", "forestgreen"))
palette(gradient(3))
trtcols <- palette(gradient(3))
boxlabs <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")

# windows(8, 12)

jpeg(filename = "output/PLC.jpeg",
     width = 10, height = 12, units = "in", res= 400)

par(mfrow=c(3,1), las=1, cex.axis=1, cex.lab=1, cex=1.25,
    mgp=c(2.5,1,0),oma=c(5, 0, 1,1))

par(mar=c(.5,5.5,.5,0))
plot(epiphyte, ylab = "", cicol=alpha(trtcols[1], .75), xlab="", ylim=c(0,101),
     pch=21, bg=alpha(trtcols[1], .5), xlim=c(0,4.1),xaxt='n',px_ci_label = FALSE,
     what='embol')
axis(side=1, labels=FALSE, tick=TRUE)
legend(x=3.3, y=100,boxlabs, pch=21, pt.bg=trtcols, bty='n',cex=1, pt.cex = 1,
       inset=-.02)
text(x=2.5,y=95,  bquote(bold(P[50] == .(round(epi_p50[[2]],2)))), cex=1)

par(mar=c(.5,5.5,.5,0))
plot(hemi_epi, cicol=alpha(trtcols[2], .75), xlab="",ylim=c(0,101),
     pch=21, bg=alpha(trtcols[2], .5), xlim=c(0,4.1), xaxt='n', ylab="",
     px_ci_label = FALSE, what='embol')
axis(side=1, labels=FALSE, tick=TRUE)
text(x=-.6, y=50, PLC_lab, srt=90, xpd=NA)
text(x=2.5,y=95,  bquote(bold(P[50] == .(round(hemi_p50[[2]],2)))), cex=1)

par(mar=c(.5,5.5,.5,0))
plot(terrestrial, ylab = "", cicol=alpha(trtcols[3], .75), ylim=c(0,101),
     pch=21, bg=alpha(trtcols[3], .5), xlim=c(0,4.1),
     px_ci_label = FALSE, what='embol')
mtext("Water Potential  (MPa)", side=1, line=3, cex=1.25)
text(x=2.5,y=95,  bquote(bold(P[50] == .(round(terr_p50[[2]],2)))), cex=1)

dev.off()
