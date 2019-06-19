
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
#0.2236678 0.8169862

#niche2   12.7224  2   0.001727

visreg(sla_mod2)
##slightly higher SD at las cruces

tukey_sla <- glht(sla_mod2, linfct = mcp(niche2 = "Tukey"))
sla_siglets <-cld(tukey_sla)

#terrestrial hemi-epiphyte      epiphyte 
#     "a"          "ab"           "b"

terr <- mean(sla[sla$niche2 == "terrestrial", "lma"]) #129.3988
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



