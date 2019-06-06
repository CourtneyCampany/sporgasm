source("functions_packages/basic_functions.R")
source("master_scripts/plot_objects.R")

library(mgcv) #for gam fits
library(scales)

##raw data points
pv_data <- read.csv("calculated_data/pv_curves_full.csv")
pv_data$niche2<-factor(pv_data$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##plotting curves
psi2lab <- expression(1/Psi~~(MPa^-1))
op_lab <- expression(paste(Psi[o], "  (MPa)"))
tlp_lab <- expression(paste(Psi[TLP], "  (MPa)"))

## trim weird data
pv_data2 <- pv_data[pv_data$rwc_100 < 15 & pv_data$rwc_100 >= 0,]
pv_data3 <- pv_data[pv_data$rwc_100 < 10 & pv_data$rwc_100 >= 0 &
                      pv_data$psi2 < 40,]
pv_data4 <- pv_data3[complete.cases(pv_data3$psi2),]

terr <- pv_data4[pv_data4$niche2 == "terrestrial",]
hemi <- pv_data4[pv_data4$niche2 == "hemi-epiphyte",]
epi <- pv_data4[pv_data4$niche2 == "epiphyte",]

#try NLS
#nls(y~a*exp(b*x), df, start=list(a= 5, b=0.04))

#this is hidden at bottom for code of exponeital


#try loess
loess_terr <- loess(psi2 ~ rwc_100, data=terr, span=.5)
terr$smooth_terr <- predict(loess_terr)
# pred_terr_se <-predict(loess_terr, se=T)

loess_hemi<- loess(psi2 ~ rwc_100, data=hemi, span=.5)
hemi$smooth_hemi <- predict(loess_hemi)

loess_epi <- loess(psi2 ~ rwc_100, data=epi, span=.5)
epi$smooth_epi <- predict(loess_epi)
  
#build plot by habitat
windows()
par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,11),
     ylim=c(0,20), ylab= psi2lab, xlab = "100-RWC ( %)")
points(psi2~rwc_100, data=pv_data4 , pch=16, col=trtcols3[niche2],
       cex=1.25)

lines(x=terr[order(terr$rwc_100),"rwc_100"], 
      y=terr[order(terr$rwc_100),"smooth_terr"], type='l',
      lwd=4, lty=2, col=trtcols[1])

lines(x=hemi[order(hemi$rwc_100),"rwc_100"], 
      y=hemi[order(hemi$rwc_100),"smooth_hemi"], type='l',
      lwd=4, lty=2, col=trtcols[2])

lines(x=epi[order(epi$rwc_100),"rwc_100"], 
      y=epi[order(epi$rwc_100),"smooth_epi"], type='l',
      lwd=4, lty=2,col=trtcols[3])




#run an exponentialmodel for each habitat

# #1)terrestrial
# mterr <- nls(psi2~a*exp(b*rwc_100), terr, start=list(a=4, b=0.04))
# #predict from model, this will become regression line
# terr$pred <- predict(mterr)
#   se_terr = summary(mterr)$sigma
#   ci_terr = outer(terr$pred, c(outer(se_terr, c(-1,1), '*'))*1.96, '+')
#   ii_terr = order(terr$rwc_100)
# 
# 
# #2) hemi
# mhemi <- nls(psi2~a*exp(b*rwc_100), hemi, start=list(a=7, b=0.04))
# hemi$pred <- predict(mhemi)
#   se_hemi = summary(mhemi)$sigma
#   ci_hemi = outer(hemi$pred, c(outer(se_hemi, c(-1,1), '*'))*1.96, '+')
#   ii_hemi = order(hemi$rwc_100)
#   
# #3) epi
#  mepi <- nls(psi2~a*exp(b*rwc_100), epi, start=list(a=7, b=0.04))
#  epi$pred <- predict(mepi)
#   se_epi = summary(mepi)$sigma
#   ci_epi = outer(epi$pred, c(outer(se_epi, c(-1,1), '*'))*1.96, '+')
#   ii_epi = order(epi$rwc_100)  
#   


# with(pv_dfr[ii,], plot(rwc_100, pred, ylim=range(ci), type='l'))
# matlines(pv_dfr[ii,'rwc_100'], ci[ii,], lty=2, col=1)
# points(psi2~rwc_100, data=pv_data3 , pch=16, col=trtcols2[niche2],
#        cex=1.25)


# plot(psi2~rwc_100, data=pv_data3 , type='n',xlim=c(0,11),
#      ylim=c(-1,15), ylab= psi2lab, xlab = "100-RWC ( %)")