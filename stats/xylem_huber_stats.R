#huber by niche
huber <- read.csv("calculated_data/xylem_area_huber.csv")
huber$niche2<-factor(huber$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
huber$id <- paste(huber$genusspecies, huber$plant_no, sep="-")

#stats for xylem area and huber values

library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

#data check:

boxplot(xylem_area_mm2 ~ niche2, data=huber)
boxplot(huber ~ niche2, data=huber)
hist(huber$xylem_area_mm2)
hist(huber$huber)

#lots of outliers for huber:
huber2 <- huber[huber$huber <= 1.147990e-05, ]
boxplot(huber ~ niche2, data=huber2)
hist(huber2$huber) #much better

#there are still a few epi outliers, so lets try to remove rownumbers
huber3 <- huber2[!(huber2$id %in% c("elalat-4", "aspjug-3","phlaur-4", 
                                    "elaher-2", "elalat-1")),]
boxplot(huber ~ niche2, data=huber3)

xylem2 <- huber[huber$xylem_area_mm2 < 0.8,]
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

#niche2      11.4126  2   0.003325 ** 

# R2m       R2c
# [1,] 0.3016933 0.8853995

visreg(xa_mod2)

tukey_xa <- glht(xa_mod2, linfct = mcp(niche2 = "Tukey"))
xa_siglets <-cld(tukey_xa)

# terrestrial hemi-epiphyte      epiphyte 
# "a"          "b"           "b" 

terr <- mean(xylem2[xylem2$niche2 == "terrestrial", "xylem_area_mm2"]) #0.259551
epi_hemi <- mean(xylem2[!xylem2$niche2 == "terrestrial", "xylem_area_mm2"]) #0.1041174


###huber values ------

#run stats on LMA because it jives better with other common regrssions
hv_mod <- lmer(sqrt(huber) ~ niche2 * site + (1|species), data=huber3)
hv_mod2 <- lmer(sqrt(huber) ~ niche2 + site + (1|species), data=huber3)

plot(hv_mod)
qqPlot(residuals(hv_mod))

Anova(hv_mod, type=3) #niche effect, no site
anova(hv_mod, hv_mod2) #not different
AIC(hv_mod, hv_mod2) #model without interaction in better


Anova(hv_mod2, type="3") #only niche effect
r.squaredGLMM(hv_mod2)

# niche2       11.7107  2   0.001038 ** 

# R2m       R2c
# [1,] 0.2329479 0.7829399


visreg(hv_mod2)
##higher huber for terr

tukey_hv <- glht(hv_mod2, linfct = mcp(niche2 = "Tukey"))
hv_siglets <-cld(tukey_hv)

# terrestrial hemi-epiphyte      epiphyte 
# "b"           "a"          "a"

terr <- mean(huber2[huber2$niche2 == "terrestrial", "huber"]) #129.3988
noterr <- mean(huber2[!huber2$niche2 == "terrestrial", "huber"]) #216.4421
