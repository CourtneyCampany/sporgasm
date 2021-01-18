source("plots/plot_objects.R")
source("plots/basic_functions.R")
source("plots/ci_functions.R")
library(plotrix)
library(mgcv) #for gam fits

alldata <- read.csv("aspb_poster/plots/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))



aspb_dat <- alldata[! alldata$niche2 == "hemi-epiphyte",]
absp_means <- doBy::summaryBy(. ~ niche2, data=aspb_dat, FUN=mean, keep.names = TRUE)
