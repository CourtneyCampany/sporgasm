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
  ss2$stomatal_length_um <- ss2$guardcell_length_um*1000
  ss2$average_guardcell_width_um <- with(ss2, 
                                         ((guardcell_width1_um + guardcell_width2_um)*1000)/2)
  ss2$stomatal_size <- with(ss2, ((guardcell_width1_um*1000) + (guardcell_width2_um*1000)) *
                              (guardcell_length_um*1000))

#mean of stomatal size per plant number
ss_agg <- doBy::summaryBy(stomatal_length_um + average_guardcell_width_um +
                            stomatal_size ~ site + species + plant_no 
                          + niche2, data=ss2, FUN=mean, keep.names = TRUE)
ss_nohemi <- droplevels(ss_agg[!ss_agg$niche2 == "hemi-epiphyte",])

##stats --------

#basic stats
chisq.out.test(ss_agg$stomatal_size) #highest points are likely outliers
hist(sqrt(ss_agg$stomatal_size)) ## likely affect frequency distribution

#simple model check
# ss_mod <- lm(log10(stomatal_size) ~ niche2, data=ss_agg)
# #model diagnostics
# qqPlot(residuals(ss_mod)) #log transformation works
# residualPlot(ss_mod)
# 
# visreg(ss_mod)  ## asplenium species with large stomata
# summary(ss_mod)
# anova(ss_mod)

boxplot(stomatal_size ~ niche2, data=ss_agg) #aspenium species outliers?

#test dataset without asplenium serratum
ss_noasp <- ss_agg[!ss_agg$species == "asplenium_serratum",]
boxplot(stomatal_size ~ niche2, data=ss_noasp)

#test dataset without any outliers
ss_noout <- ss_agg[ss_agg$stomatal_size < 2000,]
boxplot(stomatal_size ~ niche2, data=ss_noout)

## hemi outliers (micnic)
hemifix <- ss_noout
hemifix$id <- with(hemifix, paste(species, plant_no, sep = "-"))
hemifix2 <- hemifix[!(hemifix$id %in% c("mickelia_nicotianifolia-3", 
             "mickelia_nicotianifolia-5","mickelia_nicotianifolia-1")),]
boxplot(stomatal_size ~ niche2, data=hemifix2)

##full mixed model for stomatal size ---------
ss_mod2 <- lmer(sqrt(stomatal_size) ~ niche2 * site + (1|species), data=hemifix2)
ss_mod3 <- lmer(sqrt(stomatal_size) ~ niche2 + site + (1|species), data=hemifix2)
qqPlot(residuals(ss_mod2)) #sqrt transformation works best
plot(ss_mod2)

#model summary
anova(ss_mod2, ss_mod3) #not different
AIC(ss_mod2, ss_mod3) #model 2 is better (less than 2)
Anova(ss_mod2, type="3") #niche, no interaction
#use model with interaction (no matter the dataset, stomatal size differs)
r.squaredGLMM(ss_mod2)
#R2m       R2c
#0.1539562 0.8736596

# nniche2        6.6116  2    0.03667

tukey_ss <- glht(ss_mod2, linfct = mcp(niche2 = "Tukey"))
ss_siglets <-cld(tukey_ss)

#terrestrial hemi-epiphyte      epiphyte 
# "a"           "b"           "ab"

terr_ss <- mean(hemifix2[hemifix2$niche2 == "terrestrial", "stomatal_size"])
hemi_ss <- mean(hemifix2[hemifix2$niche2 == "hemi-epiphyte", "stomatal_size"])

## test no hemi
boxplot(stomatal_size ~ niche2, data=ss_nohemi)
ss_nohemi2 <- ss_nohemi[ss_nohemi$stomatal_size < 2000,]
boxplot(stomatal_size ~ niche2, data=ss_nohemi2)

nohemi_mod <- lmer(sqrt(stomatal_size) ~ niche2 * site + (1|species), data=ss_nohemi2)
nohemi_mod2 <- lmer(sqrt(stomatal_size) ~ niche2 + site + (1|species), data=ss_nohemi2)
qqPlot(residuals(nohemi_mod)) #sqrt transformation works best
plot(nohemi_mod)

anova(nohemi_mod, nohemi_mod2) #not different
AIC(nohemi_mod, nohemi_mod2) #model 1 is better (less than 2)
Anova(nohemi_mod, type="3") #niche, no interaction
#use model with interaction (no matter the dataset, stomatal size differs)
r.squaredGLMM(nohemi_mod)
#R2m       R2c
#0.1159828 0.8760527

# 0.07743

tukey_ss <- glht(nohemi_mod2, linfct = mcp(niche2 = "Tukey"))
ss_siglets <-cld(tukey_ss)

# #test for whether it is length or width?
# boxplot(stomatal_length_um ~ niche2, data=ss_agg)
# boxplot(average_guardcell_width_um ~ niche2, data=ss_agg)
# 
# sl_mod <- lmer(sqrt(stomatal_length_um)  ~ niche2 * site + (1|species), data=ss_agg)
# sl_mod2 <- lmer(sqrt(stomatal_length_um)  ~ niche2 + site + (1|species), data=ss_agg)
# qqPlot(residuals(sl_mod))
# 
# 
# anova(sl_mod, sl_mod2)
# AIC(sl_mod, sl_mod2) #use simple model
# Anova(sl_mod2, type="3") ##use simple model
# 
# tukey_sl <- glht(sl_mod2, linfct = mcp(niche2 = "Tukey"))
# sl_siglets <-cld(tukey_sl)
# 
# ##width
# 
# sw_mod <- lmer(average_guardcell_width_um  ~ niche2 * site + (1|species), 
#                data=ss_noout)
# sw_mod2 <- lmer(average_guardcell_width_um  ~ niche2 + site + (1|species), 
#                data=ss_noout)
# qqPlot(residuals(sw_mod))
# 
# Anova(sw_mod, type="3") ##interaction is not significant
# anova(sw_mod, sw_mod2)
# AIC(sw_mod, sw_mod2) #us interaction model
# 
# Anova(sw_mod, type="3")
# 
# tukey_sw <- glht(sw_mod2, linfct = mcp(niche2 = "Tukey"))
# sw_siglets <-cld(tukey_sw)
# 
# # terrestrial hemi-epiphyte      epiphyte 
# # "a"           "ab"           "b" 
# 
# terr_sw <- mean(ss_noout[ss_noout$niche2 == "terrestrial", "average_guardcell_width_um"])
# #11.29
# terr_sw_se <- se(ss_noout[ss_noout$niche2 == "terrestrial", "average_guardcell_width_um"])
# #0.229
# 
# epi_sw <- mean(ss_noout[!ss_noout$niche2 == "epiphyte", "average_guardcell_width_um"])
# #11.67
# epi_sw_se <- se(ss_noout[!ss_noout$niche2 == "epiphyte", "average_guardcell_width_um"])
# #0.20
# 
# mean(ss_noout$stomatal_length_um) 
# se(ss_noout$stomatal_length_um)
# ## diff in width:
# (noterr_sw-terr_sw)/noterr_sw
# 
# #length
# 
# terr_sl <- mean(ss_noout[ss_noout$niche2 == "terrestrial", "guardcell_length_um"])
# #48.1
# 
# epi_sl <- mean(ss_noout[ss_noout$niche2 == "epiphyte", "guardcell_length_um"])
# #45.9um
# 
# ##if use interaction model::
# # library(emmeans)
# # emmip(sw_mod, niche2 ~ site) 
# # ss_contrasts <- emmeans(sw_mod, pairwise ~ niche2)
# # ss_contrasts <- emmeans(sw_mod, pairwise ~ niche2|site)
# #if ignore site effect, then terrestrial smaller than epiphyte
# 
# ##looks like terrestrial only lower at la selva, all equal at las cruces
