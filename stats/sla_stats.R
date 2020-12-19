
sla <- read.csv("calculated_data/fern_sla.csv")
#climbed already properly classified in niche2
#reorder from ground to canopy 
  sla$niche2<-factor(sla$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  sla$lma <- with(sla, 1/(sla_cm2g/10000)) #g m-2

##run stats------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)
library(lme4)
library(MuMIn)
library(lmerTest)
library(LMERConvenienceFunctions)

# Ltest_frond <- leveneTest(sla_cm2g ~ niche2 , data = sla)

##full mixed model---------------------
boxplot(sla_cm2g ~ niche2, data=sla)
boxplot(lma ~ niche2, data=sla)
hist(sla$sla_cm2g)
hist(sla$lma)

#run stats on LMA because it jives better with other common regrssions
sla_mod2 <- lmer(log10(lma) ~ niche2 * site + (1|species), data=sla)
sla_mod3 <- lmer(log10(lma) ~ niche2 + site + (1|species), data=sla)

plot(sla_mod2)
qqPlot(residuals(sla_mod2))

#model summary
anova(sla_mod2, sla_mod3) #not different
AIC(sla_mod2, sla_mod3) #model with interaction in better

#use model with interaction
# summary(sla_mod3)
Anova(sla_mod2, type="3") #only niche effect
r.squaredGLMM(sla_mod2)
#R2m       R2c
#0.2265056 0.8169017

#niche2   0.001536


tukey_sla <- glht(sla_mod2, linfct = mcp(niche2 = "Tukey"))
sla_siglets <-cld(tukey_sla)

#terrestrial hemi-epiphyte      epiphyte 
#     "a"          "ab"           "b"

terr <- mean(sla[sla$niche2 == "terrestrial", "lma"]) #127.95
epi <- mean(sla[sla$niche2 == "epiphyte", "lma"]) #216.4421

### custom quantiles for paper
terr <- sla[sla$niche2 == "terrestrial",]
epi <- sla[sla$niche2 == "epiphyte",]
hemi <- sla[sla$niche2 == "hemi-epiphyte",]
notterr <- sla[!sla$niche2 == "epiphyte",]

b20epi <- nrow(epi[epi$stipe_length_cm <= 20 ,])
a20epi <- nrow(epi[epi$stipe_length_cm > 20 ,])
quantile(terr$lma)
quantile(epi$lma)
quantile(hemi$lma)
quantile(notterr$lma)

b20terr <- nrow(epi[epi$lma <= 292.5 ,])
a20terr <- nrow(epi[epi$lma > 292.5 ,])

b20hemi <- nrow(hemi[hemi$stipe_length_cm <= 20 ,])
a20hemi <- nrow(hemi[hemi$stipe_length_cm > 20 ,])


##For reviewer1


selva <- sla[sla$site == "la_selva",]
cruces <- sla[sla$site == "las_cruces" ,]

selva_mod <- lmer(log10(lma) ~ niche2  + (1|species), data=selva)
cruces_mod <- lmer(log10(lma) ~ niche2 + (1|species), data=cruces)

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
