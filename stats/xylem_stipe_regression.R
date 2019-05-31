#regression between stipe length and xylem area


alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata2 <- alldata[alldata$xylem_area_mm2 < .8,]

library(visreg)
library(multcomp)
library(smatr)
library(emmeans)
library(car)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)


arealength <- lmer(xylem_area_mm2 ~ stipe_length_cm * niche2  + (1|species),
                   data=alldata2)

Anova(arealength, type=3)
r.squaredGLMM(arealength)


#test hemi bad species
hemi_nolomlap <- alldata2[alldata2$niche2 == "hemi-epiphyte" & 
                            complete.cases(alldata2$stipe_length_cm),]
                          
                          & 
                            !alldata2$genusspecies %in% "lomjap",]
arealength_hemi <- lmer(xylem_area_mm2 ~ stipe_length_cm  + (1|species),
                   data=hemi_nolomlap)
Anova(arealength_hemi, type=3)


al_slopes <- lstrends(arealength, "niche2", var="stipe_length_cm")
pairs(al_slopes) ##terr-epi same, hemi-epi diff, 


library(emmeans)
emmip(arealength, stipe_length_cm ~ niche2)
emmeans(arealength, pairwise ~ stipe_length_cm : niche2)


arealength_mod2 <- sma(xylem_area_mm2 ~ stipe_length_cm * niche2,
                       data=alldata2, multcomp = TRUE,
                       multcompmethod='adjusted') #slopes not equal
summary(arealength_mod2)



plot(xylem_area_mm2 ~ stipe_length_cm, data=hemi_nolomlap)


plot(stipe_length_cm ~ xylem_area_mm2, data=alldata2, col=trtcols2[niche2],
     pch=16)

arealength3 <- lmer(stipe_length_cm ~ xylem_area_mm2 * niche2  + (1|species),
                   data=alldata2)
Anova(arealength)
