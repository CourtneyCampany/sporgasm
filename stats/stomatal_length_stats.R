source("functions_packages/basic_functions.R")

#stomata size stats
library(visreg)
library(lme4)
library(MuMIn)
library(multcomp)
library(car)
library(lattice)
library(outliers)

#data prep -------

#stomatal size from both sites
ss <- read.csv("raw_data/stomata_size_cec.csv")
  ss$plant_no <- gsub("[^0-9]", "", ss$individual)

#species habitats  
niche <- read.csv("raw_data/species_niches.csv")

ss2 <-  merge(ss, niche, by=c("genusspecies", "site"))
  ss2$niche2 <- gsub("climber", "hemi-epiphyte", ss2$niche)
  ss2$niche2 <- as.factor(ss2$niche2)
  #reorder from ground to canopy 
  ss2$niche2<-factor(ss2$niche2, 
                   levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))


#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um  ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)
ss_nohemi <- droplevels(ss_agg[!ss_agg$niche2 == "hemi-epiphyte",])


##stats --------

#basic stats
chisq.out.test(ss_agg$guardcell_length_um) 
hist(ss_agg$guardcell_length_um) 


##full mixed model for stomatal size ---------


# ss_mod4 <- lmer(sqrt(guardcell_length_um) ~ niche2 * site + (1|species), data=ss_nohemi)
# Anova(ss_mod4)

boxplot(guardcell_length_um ~ niche2, data=ss_agg) #aspenium species outliers?
qqPlot(residuals(ss_mod2)) #sqrt transformation works best
plot(ss_mod2)

sl <- ss_agg[ss_agg$guardcell_length_um < .066,] #remove outliers
boxplot(guardcell_length_um ~ niche2, data=sl)

ss_mod2 <- lmer(sqrt(guardcell_length_um) ~ niche2 * site + (1|species), data=sl)
ss_mod3 <- lmer(sqrt(guardcell_length_um) ~ niche2 + site + (1|species), data=sl)

#model summary
Anova(ss_mod2, type="3") #niche, no interaction
anova(ss_mod2, ss_mod3) #not different
AIC(ss_mod2, ss_mod3) #model 2 is better (less than 2)

#use model with interaction
r.squaredGLMM(ss_mod2)
#R2m       R2c
#0.06893237 0.8465831

#niche2        6.2322  2    0.04433 *
tukey_ss <- glht(ss_mod2, linfct = mcp(niche2 = "Tukey"))
ss_siglets <-cld(tukey_ss)

#terrestrial hemi-epiphyte      epiphyte 
# "a"           "a"           "a"

