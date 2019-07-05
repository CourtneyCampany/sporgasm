source("master_scripts/plot_objects.R")

ceb <- read.csv("raw_data/ceb_thickness.csv")
niche <- read.csv("raw_data/species_niches.csv")

ceb2 <- merge(ceb, niche)
ceb2$niche2 <- gsub("climber", "hemi-epiphyte", ceb2$niche)
ceb2$niche2 <- as.factor(ceb2$niche2)
ceb2$curve_id <- with(ceb2, paste(genusspecies, plant_no, sep="-"))

cebnozero <- ceb2[-16,] #drop asplenium #6 with 0.0

#stats on niche
library(visreg)
library(multcomp)
library(car)
library(lme4)
library(MuMIn)

boxplot(ceb_thickness_um ~ niche2, data=ceb2) 
hist(sqrt(ceb2$ceb_thickness_um) )#large outliers probably (sqrt fixes)

ceb_mod <- lmer(sqrt(ceb_thickness_um) ~ niche2 * site + (1|species), 
                   data=ceb2)
ceb_mod2 <- lmer(sqrt(ceb_thickness_um) ~ niche2 + site + (1|species), 
                    data=ceb2)

ceb_mod3 <- lmer(sqrt(ceb_thickness_um) ~ niche2 + site + (1|species), 
                 data=cebnozero)

#model diagnostics
qqPlot(residuals(ceb_mod)) #pretty good
plot(ceb_mod) ##negative skewed so we need a transformation

#model summary
Anova(ceb_mod3, type="3") #no interactions
anova(ceb_mod, ceb_mod2) #not different
AIC(ceb_mod, ceb_mod2) ##keep interaction model

#use model without interaction
summary(ceb_mod)
Anova(ceb_mod, type="3")
r.squaredGLMM(ceb_mod)
#R2m       R2c
#0.2179662 0.9008939

# thicker CEB in epiphytes

visreg(ceb_mod3)
##slightly higher SD at las cruces

tukey_ceb <- glht(ceb_mod3, linfct = mcp(niche2 = "Tukey"))
ceb_siglets <-cld(tukey_ceb)

#tukeys depends heavily on asplenium 0.000


#test correlations
ferndat <- read.csv("calculated_data/ferns_traits_complete.csv")

ceb_regress <- merge(ceb2, ferndat, all=TRUE)
ceb_regress2 <- ceb_regress[complete.cases(ceb_regress$ceb_thickness_um),]

#basic plots pressure volume curves
par(mar=c(4,4,1,1))
plot(waterpot_tlp ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(osmotic_potential ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(capacitance_full ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(elasticity ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)

#basic plots morphology
plot(lamina_area_cm2 ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(frond_length_cm ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(stipe_length_cm ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(elasticity ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(sla_cm2g ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)

#basic plots anatomy
plot(xylem_area_mm2 ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(stomatal_size ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(sd_mm2 ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)

#basic plots chemistry
plot(d13C ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)
plot(n_perc ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=ceb_regress2)

#basic plots vcurve
kmax <- read.csv("calculated_data/lascruces_kmax.csv")
kmax2 <- merge(kmax, ceb2)

#####overlap with curve id for vcurves: start here

p50 <- read.csv("calculated_data/species_p50.csv")
names(p50)[1] <- "curve_id"
p50dat <- p50[p50$x==50,]
p50_ceb <- merge(ceb2[,c(1,4:5,7,9:10)], p50dat)


plot(K ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=kmax2) #maybe for terrestrial

plot(Px ~ ceb_thickness_um,pch=16, col=trtcols2[niche2],
     data=p50_ceb) ##possible

#bivariate mixed model
p50_ceb_mod <- lmer(Px ~ ceb_thickness_um  * niche2 
                 + (1|species),  data=p50_ceb)

