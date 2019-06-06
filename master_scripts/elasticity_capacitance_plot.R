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

#try NLS

m0 <- nls(capacitance_full~a*exp(b*elasticity), pv_dfr, 
          start=list(a= 5, b=0.04))

pv_dfr$pred <- predict(m0)

se = summary(m0)$sigma
ci = outer(pv_dfr$pred, c(outer(se, c(-1,1), '*'))*1.96, '+')
ii = order(pv_dfr$elasticity)

with(pv_dfr[ii,], plot(elasticity, pred, ylim=range(ci), type='l'))
matlines(pv_dfr[ii,'elasticity'], ci[ii,], lty=2, col=1)
points(capacitance_full ~ elasticity, data=pv , pch=16, col=trtcols2[niche2],
       cex=1.25)

#plotting -----
windows()

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1)

plot(capacitance_full ~ elasticity, data=pv, type='n')

smoothplot(elasticity, capacitance_full, data=terr, kgam=3, 
           linecol=trtcols[1],pch="", add=TRUE)

smoothplot(elasticity, capacitance_full, data=hemi, kgam=3,
           linecol=trtcols[2],pch="", add=TRUE)

smoothplot(elasticity, capacitance_full, data=epi, kgam=3,
           linecol=trtcols[3],pch="", add=TRUE)

legend("topright", boxlabs, pch=16, col=trtcols, bty='n', inset = .01)


points(capacitance_full ~ elasticity, data=pv , pch=16, col=trtcols2[niche2],
       cex=1.25)




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