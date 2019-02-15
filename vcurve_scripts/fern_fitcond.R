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
ggplot(fern_plc_curves, aes(MPa,K)) +
  geom_line() +
  facet_wrap(~curve_id) +
  theme_minimal()
# it is obvious that K does not plateau at high water potentials,
# this means a typical S-curve cannot be fit


# for fitcond you have to specify what you think is the asymptoting value,
# i.e. K at a water potential of zero. There can be lots of debate about this,
# and the water physiologists often disagree!
# Here I use rescale_Px=TRUE, see ?fitcond, and fit a non-parametric curve
# because your data are not S-shaped

#create easy object for package required variables names
var_names <- c(K = "K", WP = "MPa")

# gloabl fits by habitats

#1. epiphyte
epiphyte <- fitcond(fern_plc_curves[fern_plc_curves$niche2 == "epiphyte",], 
                    model = "loess", 
                    rescale_Px=TRUE, 
                    varnames=var_names,
                    WP_Kmax=0.00)
plot(epiphyte)
epi_p50 <- getPx(epiphyte,  rescale_Px=TRUE)
getPx(epiphyte,  rescale_Px=TRUE, x=88)

#2. terrestrial
terrestrial <- fitcond(fern_plc_curves[fern_plc_curves$niche2 == "terrestrial",], 
                    model = "loess", 
                    rescale_Px=TRUE, 
                    varnames=var_names,
                    WP_Kmax=0.00)
plot(terrestrial)
terr_p50 <- getPx(terrestrial,  rescale_Px=TRUE)
getPx(terrestrial,  rescale_Px=TRUE, x=88)

#3. hemiepiphyte
hemi_epi <- fitcond(fern_plc_curves[fern_plc_curves$niche2 == "hemi-epiphyte",], 
                       model = "loess", 
                       rescale_Px=TRUE, 
                       varnames=var_names,
                       WP_Kmax=0.00)
plot(hemi_epi)
hemi_p50 <- getPx(hemi_epi,  rescale_Px=TRUE)
getPx(hemi_epi,  rescale_Px=TRUE, x=88)

## global plotting by habitat
# see ?plot.fitplc for options
k_lab <- expression(atop(Conductivity,(mg~mm~KPa^-1~s^-1)))
k_lab2 <- expression(Conductivity~~(mg~mm~KPa^-1~s^-1))
p50_lab <- expression(P[50])

#habitat colors
gradient <- colorRampPalette(c("orange", "forestgreen"))
palette(gradient(3))
trtcols <- palette(gradient(3))
boxlabs <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")

# windows(8, 12)

jpeg(filename = "output/p50.jpeg",
     width = 10, height = 12, units = "in", res= 400)

par(mfrow=c(3,1), las=1, cex.axis=1, cex.lab=1, cex=1.25,
    mgp=c(2.5,1,0),oma=c(5, 0, 1,1))

par(mar=c(.5,5.5,.5,0))
plot(epiphyte, ylab = "", cicol=alpha(trtcols[1], .75), xlab="", ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[1], .5), xlim=c(0,4.1),xaxt='n',px_ci_label = FALSE)
axis(side=1, labels=FALSE, tick=TRUE)
legend(x=3.3, y=13,boxlabs, pch=21, pt.bg=trtcols, bty='n',cex=1, pt.cex = 1,
       inset=-.02)
text(x=2.6,y=10,  bquote(bold(P[50] == .(round(epi_p50[[2]],2)))), cex=1)

par(mar=c(.5,5.5,.5,0))
plot(hemi_epi, cicol=alpha(trtcols[2], .75), xlab="",ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[2], .5), xlim=c(0,4.1), xaxt='n', ylab="",
    px_ci_label = FALSE)
axis(side=1, labels=FALSE, tick=TRUE)
text(x=-.6, y=6, k_lab2, srt=90, xpd=NA)
text(x=2.6,y=10,  bquote(bold(P[50] == .(round(hemi_p50[[2]],2)))), cex=1)

par(mar=c(.5,5.5,.5,0))
plot(terrestrial, ylab = "", cicol=alpha(trtcols[3], .75), ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[3], .5), xlim=c(0,4.1),
    px_ci_label = FALSE)
mtext("Water Potential  (MPa)", side=1, line=3, cex=1.25)
text(x=2.6,y=10,  bquote(bold(P[50] == .(round(terr_p50[[2]],2)))), cex=1)

dev.off()

# As there are a reasonable # of species, i will run by species
# that way I can inspect each curve and look for bad data

species <- unique(fern_plc_curves$genusspecies)

#lommax = yes
lommax <- fitconds(fern_plc_curves[fern_plc_curves$genusspecies == "lommax",],
                   "curve_id",model = "loess", rescale_Px=TRUE, varnames=var_names)
lommax_p50 <- getPx(lommax,  rescale_Px=TRUE)
#lomjap = yes
lomjap <- fitconds(fern_plc_curves[fern_plc_curves$genusspecies == "lomjap",], 
                   "curve_id", model = "loess", rescale_Px=TRUE, varnames=var_names)
lomjap_p50 <- getPx(lomjap,  rescale_Px=TRUE)
#cycsem = yes
cycsem <- fitconds(fern_plc_curves[fern_plc_curves$genusspecies == "cycsem",], 
                   "curve_id",model = "loess", rescale_Px=TRUE, varnames=var_names)
cycsem_p50 <- getPx(cycsem,  rescale_Px=TRUE)
#cycsem = yes
cycsem <- fitconds(fern_plc_curves[fern_plc_curves$genusspecies == "cycsem",], 
                   "curve_id",model = "loess", rescale_Px=TRUE, varnames=var_names)
cycsem_p50 <- getPx(cycsem,  rescale_Px=TRUE)
#cycsem = yes


#examine fits
par(mfrow=c(2,3))
plot(lomjap)

#start to merge these, can pick up where left off
species_p50 <- dplyr::bind_rows(lommax_p50, lomjap_p50, cycsem_p50)
write.csv(species_p50, "calculated_data/species_p50.csv", row.names = FALSE)

#use fit cond to diagnose problems with indvidual curves
#problems: olecos6,tecinc5, polosm2,3, dipro1,4,5,6,7
test <- fitcond(fern_plc_curves[fern_plc_curves$curve_id == "polosm-3",], 
                     model = "loess", rescale_Px=TRUE, varnames=var_names)

test_p50 <- getPx(test,  rescale_Px=TRUE)

par(mar=c(4,4,1,1))
plot(K ~ MPa, data=fern_plc_curves[fern_plc_curves$curve_id == "polosm-2",],
     col="black", pch=19)



