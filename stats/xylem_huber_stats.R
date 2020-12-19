#huber by niche
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
  alldata$id <- paste(alldata$genusspecies, alldata$plant_no, sep="-")
  #reorder from ground to canopy 
  alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]
  #remove outliers detected in stats (lomjap-hemi)
  alldata3 <- alldata2[! alldata2$id  %in% c("lomjap-4","lomjap-3","lomjap-6"),]

  alldata3$xylemfrac <- (alldata3$xylem_area_mm2 / (alldata3$lamina_area_cm2 * 100))*10000
  #xylemfrac is unitless standardized stem xylem measurement (huber value)
  #multiple x 100000 for ease of view

boxplot(xylemfrac~niche2, data=alldata3, outline=FALSE)

#stats for xylem area and xylemfrac values

library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

#data check:

boxplot(xylem_area_mm2 ~ niche2, data=alldata3)
hist(alldata3$xylem_area_mm2)

boxplot(xylemfrac~niche2, data=alldata3)
#lots of outliers for huber:
xylemfrac_dat <- alldata3[alldata3$xylemfrac <= .12, ]
boxplot(xylemfrac ~ niche2, data=xylemfrac_dat)
hist(xylemfrac_dat$xylemfrac) # better

#there are still a few epi outliers, so lets try to remove rownumbers
xylemfrac_dat2 <- xylemfrac_dat[!(xylemfrac_dat$id %in% 
                                    c("elalat-4", "aspjug-3","phlaur-4", 
                                    "elaher-2", "elalat-1")),]
boxplot(xylemfrac ~ niche2, data=xylemfrac_dat2)

#xylem data
xylem2 <- alldata3[alldata3$xylem_area_mm2 < 0.8,]
  boxplot(xylem_area_mm2 ~ niche2, data=xylem2)
xylem3 <- xylem2[! xylem2$id  %in% c("lomjap-4","lomjap-3","lomjap-6"),]
  boxplot(xylem_area_mm2 ~ niche2, data=xylem3)
  hist(sqrt(xylem3$xylem_area_mm2)) #going to need a transformation

#mixed model

xa_mod <- lmer(sqrt(xylem_area_mm2) ~ niche2 * site + (1|species), data=xylem3)
xa_mod2 <- lmer(sqrt(xylem_area_mm2) ~ niche2 + site + (1|species), data=xylem3)

plot(xa_mod)
qqPlot(residuals(xa_mod))

Anova(xa_mod, type=3) 
anova(xa_mod, xa_mod2) #not different
AIC(xa_mod, xa_mod2) #model without interaction in better


Anova(xa_mod2, type="3") #only niche effect
r.squaredGLMM(xa_mod2)

#niche2     0.0003748 ** 

# R2m       R2c
# [1,] 0.2913789 0.8849976

visreg(xa_mod2)

tukey_xa <- glht(xa_mod2, linfct = mcp(niche2 = "Tukey"))
xa_siglets <-cld(tukey_xa)

# terrestrial hemi-epiphyte      epiphyte 
# "a"          "ab"           "b" 

terr <- mean(xylem3[xylem3$niche2 == "terrestrial", "xylem_area_mm2"], na.rm=TRUE) #0.2459
epi <- mean(xylem2[xylem2$niche2 == "epiphyte", "xylem_area_mm2"], na.rm=TRUE) #0.087


###huber values ------

hv_mod <- lmer(sqrt(xylemfrac) ~ niche2 * site + (1|species), data=xylemfrac_dat2)
hv_mod2 <- lmer(sqrt(xylemfrac) ~ niche2 + site + (1|species), data=xylemfrac_dat2)

plot(hv_mod)
qqPlot(residuals(hv_mod))

anova(hv_mod, hv_mod2) #not different
AIC(hv_mod, hv_mod2) #model without interaction in better

Anova(hv_mod2, type="3") #only niche effect
r.squaredGLMM(hv_mod2)

boxplot(xylemfrac ~ niche2, data=xylemfrac_dat2)

# niche2     0.0231

# R2m       R2c
# [1,] 0.1436604 0.7877202


visreg(hv_mod2)
##higher huber for terr

tukey_hv <- glht(hv_mod2, linfct = mcp(niche2 = "Tukey"))
hv_siglets <-cld(tukey_hv)

# terrestrial hemi-epiphyte      epiphyte 
# "b"           "a"          "a"

terr <- mean(xylemfrac_dat2[xylemfrac_dat2$niche2 == "terrestrial", "xylemfrac"], na.rm=TRUE) 
#.04405
noterr <- mean(xylemfrac_dat2[!xylemfrac_dat2$niche2 == "terrestrial", "xylemfrac"], na.rm=TRUE) 
#.02688
#39%



##For reviewer1


selva <- xylem3[xylem3$site == "la_selva",]
cruces <- xylem3[xylem3$site == "las_cruces" ,]

selva_mod <- lmer(sqrt(xylem_area_mm2) ~ niche2  + (1|species), data=selva)
cruces_mod <- lmer(sqrt(xylem_area_mm2) ~ niche2 + (1|species), data=cruces)

#laselva
plot(selva_mod)
qqPlot(residuals(selva_mod))
summary(selva_mod)
Anova(selva_mod, type="3") #only niche effect
r.squaredGLM(selva_mod)
tukey_selva <- glht(selva_mod, linfct = mcp(niche2 = "Tukey"))
cld(tukey_selva) #same

#lascruces
plot(cruces_mod)
qqPlot(residuals(cruces_mod))
summary(cruces_mod)
Anova(cruces_mod, type="3") #only niche effect
r.squaredGLM(cruces_mod)
tukey_cruces <- glht(cruces_mod, linfct = mcp(niche2 = "Tukey"))
cld(tukey_cruces) #not same
