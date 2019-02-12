
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
                    varnames=var_names)
plot(epiphyte)
getPx(epiphyte,  rescale_Px=TRUE)
getPx(epiphyte,  rescale_Px=TRUE, x=88)

#2. terrestrial
terrestrial <- fitcond(fern_plc_curves[fern_plc_curves$niche2 == "terrestrial",], 
                    model = "loess", 
                    rescale_Px=TRUE, 
                    varnames=var_names)
plot(terrestrial)
getPx(terrestrial,  rescale_Px=TRUE)
getPx(terrestrial,  rescale_Px=TRUE, x=88)

#3. hemiepiphyte
hemi_epi <- fitcond(fern_plc_curves[fern_plc_curves$niche2 == "hemi-epiphyte",], 
                       model = "loess", 
                       rescale_Px=TRUE, 
                       varnames=var_names)
plot(hemi_epi)
getPx(hemi_epi,  rescale_Px=TRUE)
getPx(hemi_epi,  rescale_Px=TRUE, x=88)

# see ?plot.fitplc for options
k_lab <- expression(Conductivity~~(mg~mm~KPa^-1~s^-1))
#habitat colors
gradient <- colorRampPalette(c("forestgreen","orange"))
palette(gradient(3))
trtcols <- palette(gradient(3))
boxlabs <- c("Epiphyte", "Hemi-epiphyte", "Terrestrial")

windows()
par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, 
    mgp=c(2.5,1,0),oma=c(5, 5, 1,1), xpd=TRUE)

par(mar=c(.5,0,.5,0))
plot(epiphyte, ylab = "", cicol=alpha(trtcols[3], .75), xlab="", ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[3], .5), xlim=c(0,4.1),xaxt='n')
legend("topright",boxlabs, pch=21, pt.bg=trtcols,
       inset=.01,  bty='n',cex=1.25)
axis(side=1, labels=FALSE, tick=TRUE)

par(mar=c(.5,0,.5,0))
plot(hemi_epi, ylab = "", cicol=alpha(trtcols[2], .75), xlab="",ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[2], .5), xlim=c(0,4.1), xaxt='n')
axis(side=1, labels=FALSE, tick=TRUE)
text(x=tlab, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),

par(mar=c(.5,0,.5,0))
plot(terrestrial, ylab = "", cicol=alpha(trtcols[1], .75), ylim=c(0,12.5),
    pch=21, bg=alpha(trtcols[1], .5), xlim=c(0,4.1))
axis(side=1, labels="Water Potential  (MPa)")
mtext("Water Potential  (MPa)", side=1, line=3, cex=1.25)



# now by  individual species
# this takes a while because every curve is bootstrapped
# (I think this overestimates the variance of these fits by the way,
# i can look into it)
f2 <- fitconds(laselva, "curve_id",
               model = "loess", rescale_Px=TRUE, varnames=var_names)

par(mfrow=c(3,4))
plot(f2)
