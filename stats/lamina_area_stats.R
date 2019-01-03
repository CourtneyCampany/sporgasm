
traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  
lamina <- traits[complete.cases(traits$lamina_area_cm2),]
  
## stats on basic leaf morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(outliers)

chisq.out.test(lamina$lamina_area_cm2) #2448 is an outier
  
lamina_mod <- lm(lamina_area_cm2 ~ niche2, data=lamina[-203,])
#model diagnostics
qqPlot(lamina_mod)
plot(lamina_mod) ##negative skewed so we need a transformation
skewness(lamina_mod$residuals) 


lamina_mod2 <- lm(lamina_area_cm2^(1/3) ~ niche2, data=lamina[-203,])
  plot(lamina_mod2)
  qqPlot(lamina_mod2) #cube transformation works best

  Ltest_lamina <- leveneTest(lamina_area_cm2^(1/3) ~ niche2 , data = lamina[-203,])
  summary(Ltest_lamina) # not signficant so variance are equal

summary(lamina_mod2)
anova(lamina_mod2)
## significant effect of niche (p <0.001), r2 = .15

tukey_lamina <- glht(lamina_mod2, linfct = mcp(niche2 = "Tukey"))
lamina_siglets <-cld(tukey_lamina)
lamina_siglets2 <- lamina_siglets$mcletters$Letters
#terrestrial longer lmaina area than  epi, hemi same as epi

#check for effect of site
lamina_mod3 <- lm(lamina_area_cm2^(1/3) ~ niche2 * site,
                 data=lamina[-c(226,228),])
# plot(lamina_mod3)
# qqPlot(lamina_mod3)

summary(lamina_mod3)
anova(lamina_mod3) ##niche * site interaction on leaf area

bwplot(lamina_area_cm2 ~ niche2 | site , data = lamina[-c(226,228),]) 
visreg(lamina_mod3, "niche2", by="site", overlay=TRUE)  

lamina_siglets_mm<- glht(lamina_mod3, linfct = mcp(niche2 = "Tukey"))
  lamina_siglets_mm <-cld(lamina_siglets_mm)
  lamina_siglets2_mm <- lamina_siglets_mm$mcletters$Letters
  lamina_siglets2_mm ##same as before

##rerun model with interacationcalculated so post hoc works
lamina$nichesite <- interaction(lamina$niche2, lamina$site)  
lamina_mod4 <- lm(lamina_area_cm2^(1/3) ~ -1 + nichesite,data=lamina[-c(226,228),])
##if use -1 + or not results are same
tukey_lamina_NS <- summary(glht(lamina_mod4, linfct=mcp(nichesite="Tukey")))  
laminaNS_siglets <-cld(tukey_lamina_NS)
laminaNS_siglets2 <- laminaNS_siglets$mcletters$Letters

#terrestrial broadly have highest lamina area, regardless of site
#epiphites at las cruces have higher area than la selva, 
#otherwise epi/hemi are similar

#confirm with pairwise contrasts (use model#3)
library(emmeans)

emmip(lamina_mod3, niche2 ~ site) ##visualize interaction
emm_lamina <- emmeans(lamina_mod3, pairwise ~ niche2 | site)
emm_lamina
##sconfirms results of TUkeys