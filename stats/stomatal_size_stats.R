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
  ss2$average_guardcell_width_um <- with(ss2, 
                                       (guardcell_width1_um + guardcell_width1_um)/2)
  ss2$stomatal_size <- with(ss2, (guardcell_width1_um + guardcell_width1_um) *
                            guardcell_length_um)


#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(guardcell_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)
ss_nohemi <- droplevels(ss_agg[!ss_agg$niche2 == "hemi-epiphyte",])


##stats --------

#basic stats
chisq.out.test(ss_agg$stomatal_size) #highest points are likely outliers
hist(ss_agg$stomatal_size) ## likely affect frequency distribution

#simple model check
# ss_mod <- lm(log10(stomatal_size) ~ niche2, data=ss_agg)
# #model diagnostics
# qqPlot(residuals(ss_mod)) #log transformation works
# residualPlot(ss_mod)
# 
# visreg(ss_mod)  ## asplenium species with large stomata
# summary(ss_mod)
# anova(ss_mod)

##full mixed model for stomatal size ---------
ss_mod2 <- lmer(sqrt(stomatal_size) ~ niche2 * site + (1|species), data=ss_agg)
ss_mod3 <- lmer(sqrt(stomatal_size) ~ niche2 + site + (1|species), data=ss_agg)

ss_mod4 <- lmer(sqrt(stomatal_size) ~ niche2 * site + (1|species), data=ss_nohemi)
Anova(ss_mod4)

boxplot(stomatal_size ~ niche2, data=ss_agg) #aspenium species outliers?
qqPlot(residuals(ss_mod2)) #sqrt transformation works best
plot(ss_mod2)

#model summary
Anova(ss_mod2, type="3") #niche, no interaction
anova(ss_mod2, ss_mod3) #not different
AIC(ss_mod2, ss_mod4) #model 2 is better (less than 2)


#use model with interaction
r.squaredGLMM(ss_mod2)
#R2m       R2c
#0.1424696 0.879826

#niche2        6.2322  2    0.04433 *
tukey_ss <- glht(ss_mod2, linfct = mcp(niche2 = "Tukey"))
ss_siglets <-cld(tukey_ss)

#terrestrial hemi-epiphyte      epiphyte 
# "a"           "b"           "ab"

nohemi_ss <- mean(ss_agg[!ss_agg$niche2 == "hemi-epiphyte", "stomatal_size"])
#0.001178799
hemi_ss <- mean(ss_agg[ss_agg$niche2 == "hemi-epiphyte", "stomatal_size"])
#0.00150345

##non terrestrial are 40.4% larger
visreg(ss_mod2)


###test for whether it is length or width?
sl_mod <- lmer(sqrt(guardcell_length_um)  ~ niche2 * site + (1|species), data=ss_agg)
qqPlot(residuals(sl_mod))

Anova(sl_mod, type="3")  ###not length


sw_mod <- lmer(average_guardcell_width_um  ~ niche2 * site + (1|species), 
               data=ss_agg)
sw_mod2 <- lmer(average_guardcell_width_um  ~ niche2 + site + (1|species), 
               data=ss_agg)
qqPlot(residuals(sw_mod))

Anova(sw_mod, type="3") ##interaction is not significant
anova(sw_mod, sw_mod2)
AIC(sw_mod, sw_mod2) #us no interaction model

Anova(sw_mod2, type="3")
visreg(sw_mod2)

tukey_sw <- glht(sw_mod2, linfct = mcp(niche2 = "Tukey"))
sw_siglets <-cld(tukey_sw)

terr_sw <- mean(ss_agg[ss_agg$niche2 == "terrestrial", "average_guardcell_width_um"])
#0.01111373
noterr_sw <- mean(ss_agg[!ss_agg$niche2 == "terrestrial", "average_guardcell_width_um"])
#0.01349161



##if use interaction model::
# library(emmeans)
# emmip(sw_mod, niche2 ~ site) 
# ss_contrasts <- emmeans(sw_mod, pairwise ~ niche2)
# ss_contrasts <- emmeans(sw_mod, pairwise ~ niche2|site)
#if ignore site effect, then terrestrial smaller than epiphyte

##looks like terrestrial only lower at la selva, all equal at las cruces
