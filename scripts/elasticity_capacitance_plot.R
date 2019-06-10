source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")
library(mgcv)

pv <- read.csv("calculated_data/pv_curves2.csv")
  pv$niche2 <- gsub("climber", "hemi-epiphyte", pv$niche)
  pv$niche2 <- as.factor(pv$niche2)
  #reorder from ground to canopy 
  pv$niche2<-factor(pv$niche2, 
                    levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

terr <- pv[pv$niche2 == "terrestrial",]
hemi <- pv[pv$niche2 == "hemi-epiphyte",]
epi <- pv[pv$niche2 == "epiphyte",]

pv_dfr <- pv[,10:11]

#try loess
loess_terr <- loess(capacitance_full ~ elasticity, data=terr, span=.9)
terr$smooth_terr <- predict(loess_terr)
# pred_terr_se <-predict(loess_terr, se=T)

loess_hemi<- loess(capacitance_full ~ elasticity, data=hemi, span=.5)
hemi$smooth_hemi <- predict(loess_hemi)

loess_epi <- loess(capacitance_full ~ elasticity, data=epi, span=.5)
epi$smooth_epi <- predict(loess_epi)

#try NLS

m0 <- nls(capacitance_full~a*exp(b*elasticity), pv_dfr, 
          start=list(a= 5, b=0.04))
pv_dfr$pred <- predict(m0)

se = summary(m0)$sigma
ci = outer(pv_dfr$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(pv_dfr$elasticity)

with(pv_dfr[ii,], plot(elasticity, pred, ylim=range(ci), type='l'))
matlines(pv_dfr[ii,'elasticity'], ci[ii,], lty=2, col=1)


elas_lab <- expression(epsilon~~(MPa))
capft_lab <- expression(C[ft]~~(MPa^-1))
captlp_lab <- expression(C[TLP]~~(MPa^-1))
cap_cld <- c("a","ab","b" )

#plotting -----
windows(7,7)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)

plot(capacitance_full ~ elasticity, data=pv, type='n',xlab=elas_lab,
     ylab=capft_lab)
points(capacitance_full ~ elasticity, data=pv , pch=16, 
       col=trtcols3[niche2],
       cex=1.25)
lines(x=terr[order(terr$elasticity),"elasticity"], 
       y=terr[order(terr$elasticity),"smooth_terr"], 
       lwd=4, lty=2, col=trtcols[1])
lines(x=hemi[order(hemi$elasticity),"elasticity"], 
       y=hemi[order(hemi$elasticity),"smooth_hemi"], 
       lwd=4, lty=2, col=trtcols[2])
lines(x=epi[order(epi$elasticity),"elasticity"], 
       y=epi[order(epi$elasticity),"smooth_epi"], 
       lwd=4, lty=2, col=trtcols[3])
# legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)

par(fig=c(.4, .9, 0.45,0.90), mar=c(0,2.5,0,0),new=T, cex=1, las=1,
    cex.axis=.7, cex.lab=.7, tcl=-.25,mgp=c(1.5,.5,0))
boxplot(capacitance_zero. ~ niche2, data=pv, xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,ylim=c(0, 1.25),
        varwidth=TRUE,ylab=captlp_lab,outline=FALSE,xlab="",
        border=trtcols)
axis(1, labels=FALSE, at=1:3)
mtext(boxlabs, 1, at=1:3,line=.05, cex=.7)
stripchart(capacitance_zero. ~ niche2, data = pv,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=1.2, cap_cld)


# 
# smoothplot(elasticity, capacitance_full, data=terr, kgam=3, 
#            linecol=trtcols[1],pch="", add=TRUE)
# 
# smoothplot(elasticity, capacitance_full, data=hemi, kgam=3,
#            linecol=trtcols[2],pch="", add=TRUE)
# 
# smoothplot(elasticity, capacitance_full, data=epi, kgam=3,
#            linecol=trtcols[3],pch="", add=TRUE)


##other solution:
# xnew <- seq(min(df$x),max(df$x),0.01) #range
# RegLine <- predict(m0,newdata = data.frame(x=xnew))
# plot(df$x,df$y,pch=20)
# lines(xnew,RegLine,lwd=2)
# lines(xnew,RegLine+summary(m0)$sigma,lwd=2,lty=3)
# lines(xnew,RegLine-summary(m0)$sigma,lwd=2,lty=3)
# 
# #example with lines up to graph border
# plot(df$x,df$y,xlim=c(0,0.7),pch=20)
# xnew <- seq(par()$usr[1],par()$usr[2],0.01)
# RegLine <- predict(m0,newdata = data.frame(x=xnew))
# lines(xnew,RegLine,lwd=2)
# lines(xnew,RegLine+summary(m0)$sigma*1.96,lwd=2,lty=3)
# lines(xnew,RegLine-summary(m0)$sigma*196,lwd=2,lty=3)