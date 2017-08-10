library(lme4)
library(car)
library(gplots)
library(Hmisc)
library(lmerTest)
library(MuMIn)

mike <- read.csv("mikedata.csv")
str(mike)
summary(mike)

mike$area <- mike$Gametophyte.Width..mm. * mike$Gametophyte.Length..mm.

regal <- mike[mike$Date == 40 & mike$Species == "osm_reg",]
cin <- mike[mike$Date == 47 & mike$Species == "osm_cin",]
cla <- mike[mike$Date == 47 & mike$Species == "osm_cla",]

regal_cin <- merge(regal, cin, all=TRUE)

gam_species<- merge(regal_cin, cla, all=TRUE)

length_mod <- lmer(Gametophyte.Length..mm. ~ CO2 + Water.Potential + 
                  CO2*Water.Potential + 
                  (1|Species), data=gam_species)
summary(length_mod)
r.squaredGLMM(length_mod)
Anova(length_mod, test="F")
VarCorr(length_mod)
#these functions above will produce anova tables, r2 values p values
#in anova if interaction is not sig, remove and rerun 

# contribution of random effects to variation
nullmod <- lmer(Gametophyte.Length..mm. ~ 1 + 
                  (1|Species), data=gam_species)






library(visreg)
visreg(leafmod, "logvol", by="leaf_type", overlay=TRUE)