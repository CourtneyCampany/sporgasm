## figure 1 (morphology and chemistry) panel figure 
source("master_scripts/plot_objects.R")
source("functions_packages/ci_functions.R")

#traits data -----
traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "terrestrial", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

##separate habitat dataframes for all traits -----
terr <- traits[traits$niche2 == "terrestrial" & 
                 complete.cases(traits$stipe_length_cm),]
hemi <- traits[traits$niche2 == "hemi-epiphyte" &
                 complete.cases(traits$stipe_length_cm),]
epi <- traits[traits$niche2 == "epiphyte"&
                complete.cases(traits$stipe_length_cm),]

#log model fits for allometry plotting
terr_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + 1) ,data=terr)
hemi_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + 1) , data=hemi)
epi_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + 1) ,data=epi)

# terr_mod2 <- lm(lamina_area_cm2 ~ stipe_length_cm ,data=terr)
# hemi_mod2 <- lm(lamina_area_cm2 ~ stipe_length_cm, data=hemi)
# epi_mod2 <- lm(lamina_area_cm2 ~ stipe_length_cm ,data=epi)
fronddat <- traits[-203,] ##loses one outlier so same as stats

#plot bits-------
library(magicaxis)
library(plotrix)
stipecld <- c("a", "b", "b")
cldlma <- c("a", "ab", "b")
lma_lab <- expression(LMA~~(g~m^-2))

#plot ----------
# windows(12,8)

# jpeg(filename = "master_scripts/figure1.jpeg",
#       width = 12, height = 6, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.15, cex.axis=1.15)

# stipe length
boxplot(stipe_length_cm ~ niche2, data=traits, ylim=c(0, 82),xaxt='n',
        boxlwd=2,whisklwd=2,staplelwd=2,xlab="",
        ylab = stipe_lab,border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, at=1:3, labels=FALSE)
mtext(boxlabs, side=1, at=1:3, cex=1.15, line=2.5)
stripchart(stipe_length_cm ~ niche2, data = traits,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
text(x=1:3, y=80, stipecld, cex=1.15)
text(3.5, 0, "A", cex=1.25)

# allometry
with(fronddat, plot(log10(lamina_area_cm2) ~ log10(stipe_length_cm+1.01),
                    xlab=stipe_lab, ylab=lamina_lab,axes=FALSE,
                    pch=16, col=trtcols2[niche2],cex=1.25,
                    xlim=c(0,2.05),
                    ylim=c(1, 3.5)))
magaxis(side=c(1,2), unlog='xy', frame.plot=TRUE)
legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01, cex=1.15)
ablineclip(terr_mod, x1=log10(min(terr$stipe_length_cm+1)), 
           x2=log10(max(terr$stipe_length_cm+1)),
           col=trtcols[1], lwd=3, lty=2)
ablineclip(hemi_mod, x1=log10(min(hemi$stipe_length_cm+1)), 
           x2=log10(max(hemi$stipe_length_cm+1)),
           col=trtcols[2], lwd=3, lty=2)
ablineclip(epi_mod, x1=log10(min(epi$stipe_length_cm+1)), 
           x2=log10(max(epi$stipe_length_cm+1)),
           col=trtcols[3], lwd=3, lty=2)
text(log10(100), log10(10), "B", cex=1.25)

text(log10(150), log10(25), expression(paste(R[cond]^{"2"}," = "," 0.24")), 1.25)
text(log10(150), log10(15), expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)

# dev.off()


# with(fronddat, plot(lamina_area_cm2 ~ stipe_length_cm,
#                     xlab=stipe_lab, ylab=lamina_lab,axes=FALSE,
#                     pch=16, col=trtcols2[niche2],cex=1.25))
# magaxis(side=c(1,2),  frame.plot=TRUE)
# legend("topleft", legend = boxlabs, pch=16, col=trtcols, bty="n", inset=.01)
# ablineclip(terr_mod2, x1=min(terr$stipe_length_cm), 
#            x2=max(terr$stipe_length_cm),
#            col=trtcols[1], lwd=3, lty=2)
# ablineclip(hemi_mod2, x1=min(hemi$stipe_length_cm), 
#            x2=max(hemi$stipe_length_cm),
#            col=trtcols[2], lwd=3, lty=2)
# ablineclip(epi_mod2, x1=min(epi$stipe_length_cm), 
#            x2=max(epi$stipe_length_cm),
#            col=trtcols[3], lwd=3, lty=2)
# text(log10(250), log10(10), "A", cex=1.25)
