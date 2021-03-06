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

nitro_poster <- droplevels(nitro[nitro$niche2 != "hemi-epiphyte",])


###simple modesl for predline
terr2 <- droplevels(nitro[nitro$niche2 == "terrestrial", ])
epi2 <- droplevels(nitro[nitro$niche2 == "epiphyte", ])

terr_mod2 <- lm(log10(n_perc) ~ log10(lma_g_m2), data=terr2)
epi_mod2 <- lm(log10(n_perc) ~ log10(lma_g_m2), data=epi2)

cldlma <- c("a", "b")

#c13 and lma (eventually merge this with above)
alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                       levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))
alldata$lma <-  with(alldata, 1/(sla_cm2g/10000))

traits_poster <- droplevels(alldata[alldata$niche2 != "hemi-epiphyte",])


##separate habitat dataframes for all traits -----
terr <- alldata[alldata$niche2 == "terrestrial",]
epi <- alldata[alldata$niche2 == "epiphyte",]


c13dat <- alldata[!alldata$genusspecies == "bleschi",]
cldc13 <- c("a","b") 

boxlabs3 <- c("Terrestrial",  "Epiphyte")
trtcols_poster <- c("dodgerblue4","darkorange3")
trtcols_poster2 <- c(alpha(trtcols_poster[1], .7), alpha(trtcols_poster[2], .7))


##lma plots ------ 
library(plotrix)

jpeg(filename = "aspb_poster/plots/figure3_poster.jpeg",
       width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2), mgp=c(2,.75,0), mar=c(6,4,1,1), cex.lab=1,cex.axis=1)

boxplot(lma_g_m2 ~ niche2, data=nitro_poster,xaxt='n',ylim=c(0,630),
        boxlwd=2, whisklwd=2,staplelwd=2,xlab="",
        ylab=lma_lab, outline=FALSE, border=trtcols_poster, varwidth=TRUE)
axis(1, boxlabs3, at=1:2, cex.axis=1)
stripchart(lma_g_m2 ~ niche2, data = nitro_poster,cex=1,
           vertical = TRUE, method = "jitter",
           pch=16, col=trtcols_poster2,
           xaxt='n', add=TRUE) 
text(x=1:2, y=600, cldlma, cex=1)
text(2.5, 0, "A", cex=1.1)
mtext("Figure 3. (A) Box plots of leaf mass per unit area (LMA) across life forms.", 1, line=4, adj=0)
mtext("               (B) Negative relationships between lamina nitrogen content and LMA for ferns in this study compared to the GLOPNET data set",
      1, line=5, adj=0)

#lma v nitro
with(nitro_poster, plot(log10(n_perc) ~ log10(lma_g_m2), ylim=c(-.65,.9),xlim=c(1,3),
                 xlab = lmalab,
                 ylab = "Foliar Nitrogen (%)",
                 axes=FALSE, type='n'))

with(glopnet, points(log.Nmass ~ log.LMA,pch=16, col=globcol, cex=1.25))
points(log10(n_perc) ~ log10(lma_g_m2), data=nitro_poster[nitro_poster$lma_g_m2 < 600,], 
       pch=16, col=trtcols_poster2[niche2],cex=1.25)


ablineclip(terr_mod2, col=trtcols_poster[1], lwd=2, lty=2,
           x1=min(log10(30)), x2=max(log10(292.5)))
# 
# ablineclip(epi_mod2, col=trtcols_poster[2], lwd=2, lty=2,
#            x1=min(log10(65)), x2=max(log10(535)))

magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
text(3, -.6, "C", cex=1.1)
legend("bottomleft", legend = c(boxlabs3, "Wright et al. 2004"), pch=16, 
                                col=c(trtcols_poster,"grey50"),cex=1.15,
                                bty="n", inset=.02)

dev.off()


