
traits <- read.csv("calculated_data/fern_traits.csv")

## Climber seems to be close to terrestrial and it still is technically-------
## create new variable that adds climber to terrestrial category

traits$niche2 <- traits$niche
  traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche2)
  traits$niche2 <- as.factor(traits$niche2)
  #reorder from ground to canopy 
  traits$niche2<-factor(traits$niche2, 
                        levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
  
stipe <- traits[complete.cases(traits$stipe_length_cm),]
#stipe length has zeros, may be difficult to normalize

boxplot(stipe_length_cm ~ niche2, data=traits)
#quite a few, lets see if the matter

## stats on basic stipe morphology-------
library(visreg)
library(multcomp)
library(car)
library(lattice)
library(moments)
library(MASS)
library(outliers)
library(lme4)
library(MuMIn)

# stipe_mod <- lm(stipe_length_cm ~ niche2, data=stipe)
# 
# #model diagnostics
# qqPlot(stipe_mod) 
# chisq.out.test(stipe$stipe_length_cm) ##point 242 needs to be removed
# plot(stipe_mod) 

# stipe2 <- stipe[stipe$stipe_length_cm < 200,] #remove the outlier

# stipe_mod2 <- lm(stipe_length_cm ~ niche2, data=stipe2)
#   plot(stipe_mod2)
#   
#   qqPlot(stipe_mod2) #needs a transformation
#   skewness(stipe_mod2$residuals) #less than 1, but barely
#   kurtosis(stipe_mod2$residuals) 
#   hist(stipe_mod2$residuals)
  
  # b <- boxcox(stipe_length_cm +.1~ niche2, data=stipe2)
  # lambda <- b$x
  # lik <- b$y
  # bc <- cbind(lambda, lik)
  # sorted_bc <- bc[order(-lik),]
  # head(sorted_bc, n = 10)
  # 
  # boxcox(traits$stipe_length_cm+.1) #this is an example of how to find a lambda
  #should be sqrt
  
  # stipe_mod3 <- lm(sqrt(stipe_length_cm) ~ niche2, data=stipe2)
  # plot(stipe_mod3)
  # qqPlot(stipe_mod3)
  # 
  # summary(stipe_mod3)
  # anova(stipe_mod3)
  # 
  # Ltest_frond <- leveneTest(sqrt(stipe_length_cm) ~ niche2 , data = stipe2)
  # Ltest_frond # variances are not equal!! probably need non-parameteric
  

##go ahead and run through normally, then use kruskal wallace
# visreg(stipe_mod4, "niche2", by="site", overlay=TRUE) 

##full mixed model:
stipe_mod7 <- lmer(sqrt(stipe_length_cm) ~ niche2 * site
                   + (1|species), data=traits)

stipe_mod8 <- lmer(sqrt(stipe_length_cm) ~ niche2 + site 
                   + (1|species), data=stipe)

# boxplot(stipe_length_cm ~ niche2, data=stipe) #outliers may be present
# hist(traits$stipe_length_cm)
# plot(stipe_mod7)
# qqPlot(residuals(stipe_mod7))
# 
# #model summary
# Anova(stipe_mod7, type="3") #niche but no interaction
# anova(stipe_mod7, stipe_mod8) #not different
# AIC(stipe_mod7, stipe_mod8) #aic close, use no interaction

#use model without interaction
# summary(stipe_mod8)
Anova(stipe_mod8, type="3")
r.squaredGLMM(stipe_mod8)

# niche2       12.5416  2   0.001891 ** 

#           R2m       R2c
# [1,] 0.2874501 0.8818687

visreg(stipe_mod8, "niche2") 

tukey_stipe_8 <- summary(glht(stipe_mod8, linfct=mcp(niche2="Tukey")))  
stipe8_siglets <-cld(tukey_stipe_8)
# terrestrial hemi-epiphyte      epiphyte 
# "a"          "ab"           "b" 

terr <- mean(traits[traits$niche2 == "terrestrial", "stipe_length_cm"], na.rm=TRUE)
noterr <- mean(traits[!traits$niche2 == "terrestrial", "stipe_length_cm"], 
               na.rm=TRUE)


