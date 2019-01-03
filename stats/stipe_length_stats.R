
traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "terrestrial", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  
stipe <- traits[complete.cases(traits$stipe_length_cm),]
#stipe length has zeros, may be difficult to normalize

## stats on basic stipe morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)

stipe_mod <- lm(stipe_length_cm ~ niche2, data=stipe)

#model diagnostics
qqPlot(stipe_mod) 
chisq.out.test(stipe$stipe_length_cm) ##point 242 needs to be removed
plot(stipe_mod) 

stipe2 <- stipe[stipe$stipe_length_cm < 200,] #remove the outlier

stipe_mod2 <- lm(stipe_length_cm ~ niche2, data=stipe2)
  plot(stipe_mod2)
  
  qqPlot(stipe_mod2) #needs a transformation
  skewness(stipe_mod2$residuals) #less than 1, but barely
  kurtosis(stipe_mod2$residuals) 
  hist(stipe_mod2$residuals)
  
  # b <- boxcox(stipe_length_cm +.1~ niche2, data=stipe2)
  # lambda <- b$x
  # lik <- b$y
  # bc <- cbind(lambda, lik)
  # sorted_bc <- bc[order(-lik),]
  # head(sorted_bc, n = 10)
  # 
  # boxcox(traits$stipe_length_cm+.1) #this is an example of how to find a lambda
  #should be sqrt
  
  stipe_mod3 <- lm(sqrt(stipe_length_cm) ~ niche2, data=stipe2)
  plot(stipe_mod3)
  qqPlot(stipe_mod3)
  
  summary(stipe_mod3)
  anova(stipe_mod3)
  
  Ltest_frond <- leveneTest(sqrt(stipe_length_cm) ~ niche2 , data = stipe2)
  Ltest_frond # variances are not equal!! probably need non-parameteric
  

##go ahead and run through normally, then use kruskal wallace
  
    
stipe_mod4 <- lm(sqrt(stipe_length_cm) ~ niche2 * site, data=stipe2)
  summary(stipe_mod4)
  anova(stipe_mod4) ## no interaction, so rerunwith main effects

stipe_mod5 <- lm(sqrt(stipe_length_cm) ~ niche2 + site, data=stipe2)
  summary(stipe_mod5)
  anova(stipe_mod5)

library(emmeans)
  
  emmip(stipe_mod4, niche2 ~ site) ##visualize interaction
  emm_lamina <- emmeans(stipe_mod4, pairwise ~ niche2 | site)
  emm_lamina
  
#tukey with interaction
stipe2$nichesite <- interaction(stipe2$niche2, stipe2$site)  
stipe_mod6 <- lm(sqrt(stipe_length_cm) ~ -1 + nichesite,data=stipe2)
  
##if use -1 + or not results are same
tukey_stipe_NS <- summary(glht(stipe_mod6, linfct=mcp(nichesite="Tukey")))  
stipeNS_siglets <-cld(tukey_stipe_NS)
stipeNS_siglets2 <- stipeNS_siglets$mcletters$Letters

##terrestrial > others
##terrestrial > at las cruces

visreg(stipe_mod4, "niche2", by="site", overlay=TRUE) 
