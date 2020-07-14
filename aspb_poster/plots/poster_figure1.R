## figure 1 (morphology and chemistry) panel figure 
source("aspb_poster/plots/plot_objects.R")
source("aspb_posters/plots/ci_functions.R")

#traits data -----
traits <- read.csv("calculated_data/fern_traits.csv")
traits$niche2 <- gsub("climber", "hemi-epiphyte", traits$niche)
traits$niche2 <- as.factor(traits$niche2)

#reorder from ground to canopy 
traits$niche2<-factor(traits$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

traits_poster <- droplevels(traits[traits$niche2 != "hemi-epiphyte",])

##separate habitat dataframes for all traits -----
terr <- traits[traits$niche2 == "terrestrial" & 
                 complete.cases(traits$stipe_length_cm),]
epi <- traits[traits$niche2 == "epiphyte"&
                complete.cases(traits$stipe_length_cm),]

#log model fits for allometry plotting
terr_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + 1) ,data=terr)
epi_mod <- lm(log10(lamina_area_cm2) ~ log10(stipe_length_cm + 1) ,data=epi)


fronddat <- traits_poster[-203,] ##loses one outlier so same as stats

#plot bits-------
library(magicaxis)
library(plotrix)
stipecld <- c("a", "b")
cldlma <- c("a", "b")
lma_lab <- expression(LMA~~(g~m^-2))
boxlabs3 <- c("Terrestrial",  "Epiphyte")
trtcols_poster <- c("dodgerblue4","darkorange3")
trtcols_poster2 <- c(alpha(trtcols_poster[1], .7), alpha(trtcols_poster[2], .7))

#plot ----------
# windows(12,8)

jpeg(filename = "aspb_poster/plots/figure1_poster.jpeg",
      width = 10, height = 6, units = "in", res= 400)

par(mfrow=c(1,2),mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.15, cex.axis=1.15)

# stipe length
boxplot(stipe_length_cm ~ niche2, data=traits_poster, ylim=c(0, 82),xaxt='n',
        boxlwd=2,whisklwd=2,trtcols_poster=2,xlab="",
        ylab = stipe_lab,border=trtcols_poster, varwidth=TRUE, outline=FALSE)
axis(1, at=1:2, labels=FALSE)
mtext(boxlabs3, side=1, at=1:2, cex=1.15, line=2.5)
stripchart(stipe_length_cm ~ niche2, data = traits_poster,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols_poster2, xaxt='n', add=TRUE)
text(x=1:2, y=80, stipecld, cex=1.15)
text(2.5, 0, "A", cex=1.25)

# allometry
with(fronddat, plot(log10(lamina_area_cm2) ~ log10(stipe_length_cm+1.01),
                    xlab=stipe_lab, ylab=lamina_lab,axes=FALSE,
                    pch=16, col=trtcols_poster2[niche2],cex=1.25,
                    xlim=c(0,2.05),
                    ylim=c(1, 3.5)))
magaxis(side=c(1,2), unlog=c(1,2), frame.plot=TRUE)
legend("topleft", legend = boxlabs3, pch=16, col=trtcols_poster, bty="n", inset=.01, cex=1.15)
ablineclip(terr_mod, x1=log10(min(terr$stipe_length_cm+1)), 
           x2=log10(max(terr$stipe_length_cm+1)),
           col=trtcols_poster[1], lwd=3, lty=2)
ablineclip(epi_mod, x1=log10(min(epi$stipe_length_cm+1)), 
           x2=log10(max(epi$stipe_length_cm+1)),
           col=trtcols_poster[2], lwd=3, lty=2)
text(log10(100), log10(10), "B", cex=1.25)

text(log10(150), log10(25), expression(paste(R[cond]^{"2"}," = "," 0.23")), 1.25)
text(log10(150), log10(15), expression(paste(R[marg]^{"2"}," = "," 0.88")), 1.25)

dev.off()


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
