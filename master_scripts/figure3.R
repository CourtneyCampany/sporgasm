source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

#Leaf economic spectrum data
glopnet <- read.csv("calculated_data/glopnet.csv")
glopfern <- glopnet[glopnet$GF == "F",]
globcol <- alpha("grey50", .1)


# chemistry and lma data -----
leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
leafchem$niche2 <- gsub("climber", "terrestrial", leafchem$niche)
leafchem$niche2 <- as.factor(leafchem$niche2)

#sla data
sla <- read.csv("calculated_data/fern_sla.csv")

##merge lma and nitrogen
nitro <- merge(leafchem, sla, all=TRUE)  
nitro$lma_g_m2 <- with(nitro, 1/(sla_cm2g/10000))
nitro$nitro_area <- with(nitro,lma_g_m2 * (n_perc/100))

#reorder from ground to canopy 
nitro$niche2<-factor(nitro$niche2, 
                     levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

###simple modesl for predline
terr2 <- droplevels(nitro[nitro$niche2 == "terrestrial", ])
hemi2 <- droplevels(nitro[nitro$niche2 == "hemi-epiphyte", ])
epi2 <- droplevels(nitro[nitro$niche2 == "epiphyte", ])

# terr_mod2 <- lm(n_perc ~ lma_g_m2, data=terr2[terr2$lma_g_m2 < 600,])
# hemi_mod2 <- lm(n_perc ~ lma_g_m2, data=hemi2[hemi2$lma_g_m2 < 600,])
# epi_mod2 <- lm(n_perc ~ lma_g_m2, data=epi2[epi2$lma_g_m2 < 600,])

terr_mod2 <- lm(log10(n_perc) ~ log10(lma_g_m2), data=terr2)
hemi_mod2 <- lm(log10(n_perc) ~ log10(lma_g_m2), data=hemi2)
epi_mod2 <- lm(log10(n_perc) ~ log10(lma_g_m2), data=epi2)

cldlma <- c("a", "ab", "b")

#c13 and lma (eventually merge this with above)
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <-  with(alldata, 1/(sla_cm2g/10000))

##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
hemi <- alldata[alldata$niche2 == "hemi-epiphyte" ,]
epi <- alldata[alldata$niche2 == "epiphyte",]

#models
# terr_mod_13c <- lm(d13C ~ lma, data=terr)
# hemi_mod_13c <- lm(d13C ~ lma, data=hemi)
# epi_mod_13c <- lm(d13C ~ lma, data=epi) #no epi

c13dat <- alldata[!alldata$genusspecies == "bleschi",]
cldc13 <- c("a","a","b") 


##lma plots ------ 
library(plotrix)

# jpeg(filename = "output/figure2_lma.jpeg",
#        width = 12, height = 5, units = "in", res= 400)

layout(matrix(c(1,3,2,2), 2, 2, byrow = TRUE))

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1,cex.axis=1)

boxplot(lma_g_m2 ~ niche2, data=nitro,xaxt='n',ylim=c(0,630),
        boxlwd=2, whisklwd=2,staplelwd=2,xlab="",
        ylab=lma_lab, outline=FALSE, border=trtcols, varwidth=TRUE)
axis(1, boxlabs, at=1:3, cex.axis=1)
stripchart(lma_g_m2 ~ niche2, data = nitro,cex=1,
           vertical = TRUE, method = "jitter",
           pch=16, col=trtcols2,
           xaxt='n', add=TRUE) 
text(x=1:3, y=600, cldlma, cex=1)
text(3.5, 0, "A", cex=1.1)

#lma v nitro
with(nitro, plot(log10(n_perc) ~ log10(lma_g_m2), ylim=c(-.65,.9),xlim=c(1,3),
                 xlab = lmalab,
                 ylab = "Foliar Nitrogen (%)",
                 axes=FALSE, type='n'))

with(glopnet, points(log.Nmass ~ log.LMA,pch=16, col=globcol, cex=1.25))
points(log10(n_perc) ~ log10(lma_g_m2), data=nitro[nitro$lma_g_m2 < 600,], 
       pch=16, col=trtcols2[niche2],cex=1.25)


ablineclip(terr_mod2, col=trtcols[1], lwd=2, lty=2,
           x1=min(log10(30)), x2=max(log10(292.5)))
ablineclip(hemi_mod2, col=trtcols[2], lwd=2, lty=2,
           x1=min(log10(88)), x2=max(log10(274)))
# ablineclip(epi_mod2, col=trtcols[3], lwd=2, lty=2,
#            x1=min(log10(65)), x2=max(log10(535)))

# predline(epi_mod2, col=trtcols[3], lwd=2, lty=2)
# predline(hemi_mod2, col=trtcols[2], lwd=2, lty=2)
# predline(terr_mod2, col=trtcols[1], lwd=2, lty=2)

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
text(3, -.6, "C", cex=1.1)
legend("bottomleft", legend = c(boxlabs, "Wright et al. 2004"), pch=16, 
                                col=c(trtcols,"grey50"),cex=1.15,
                                bty="n", inset=.02)
# text(455,.2,"LMA x Niche, P < 0.001")
text(log10(800), log10(7), expression(paste(R[cond]^{"2"}," = "," 0.21")))
text(log10(800), log10(4.75), expression(paste(R[marg]^{"2"}," = "," 0.79")))

#c13
boxplot(d13C ~ niche2, data=c13dat, xaxt='n',ylim=c(-38, -25),xlab="",
        boxlwd=2, whisklwd=2,staplelwd=2,
        ylab=c13lab,border=trtcols,  varwidth=TRUE, outline=FALSE)
stripchart(d13C ~ niche2, data = c13dat,cex=1,
           vertical = TRUE, method = "jitter", 
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:3, cex.axis=1)
text(x=1:3, y=-25.5, cldc13, cex=1)
text(3.5, -38, "B", cex=1.1)

# #inset c13 site
# text(65, .01, "D", cex=1.25)
# 
# par(fig=c(.7, .95, 0.25,0.45), mar=c(0,2.5,0,0),new=T, cex=1, las=1,
#     cex.axis=.7, cex.lab=.7, tcl=-.25,mgp=c(1.5,.5,0))
# boxplot(d13C ~ site, data=c13dat, xaxt='n',
#         boxlwd=2,whisklwd=2,staplelwd=2,ylim=c(-38, -25),
#         varwidth=TRUE,ylab=c13lab,outline=FALSE,xlab="")
# axis(1, labels=FALSE, at=1:3)
# mtext(c("La Selva","Las Cruces"), 1, at=1:3,line=.05, cex=.7)
# stripchart(d13C ~ site, data=c13dat,
#            vertical = TRUE, method = "jitter",cex=.8,
#            pch = 16, xaxt='n', add=TRUE)
# text(x=1:2, y=-25.5, c("a", "b"))

 # dev.off()
