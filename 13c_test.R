source("master_scripts/plot_objects.R")
library(visreg)


alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata <-  with(alldata, 1/(sla_cm2g/10000))

plot(d13C ~ n_perc,data=alldata, col= trtcols2[niche2], pch=16)
plot(d13C ~ huber ,data=alldata[alldata$huber <0.0005,],
     col= trtcols2[niche2], pch=16)
plot(d13C ~ lma,data=alldata,`col= trtcols2[niche2], pch=16)

anova(lm(d13C ~ lamina_area_cm2 ,data=alldata))
anova(lm(d13C ~ sla_cm2g ,data=alldata[alldata$sla_cm2g < 200,]))
test <- lm(d13C ~ n_perc * niche2,data=alldata)
test2 <- lm(d13C ~ sla_cm2g * niche2,data=alldata[alldata$sla_cm2g < 200,])
visreg(test, "n_perc", by="niche2")
visreg(test2, "sla_cm2g", by="niche2")
