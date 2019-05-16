source("master_scripts/plot_objects.R")

##PCA using only means of traits, 
## pvcuurves are not from sample individuals as morphology stuff

alldata <- read.csv("calculated_data/ferns_traits_complete.csv")
#reorder from ground to canopy 
alldata$niche2<-factor(alldata$niche2, 
                      levels=c("terrestrial", "hemi-epiphyte", "epiphyte"))

#functions with na.rm allowed
se_na <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
mean_na <- function(x) mean(x, na.rm=TRUE)


alldata2 <- alldata[,-c(6:7,9, 14:17, 20:21, 24:26, 29, 32:34)]

trait_means <- doBy::summaryBy(.~ species + site + genusspecies + niche2 + group,
                               data=alldata2, FUN=mean_na, keep.names = TRUE)
trait_se <- doBy::summaryBy(.~ species + site + genusspecies + niche2 + group,
                               data=alldata2, FUN=se_na, keep.names = TRUE)


library(vegan)

#drop unneeded traits from pca
# droptraits <- c("SWC", "rwc_tlp", "capacitance_zero.", "capacitance_absolute",
#                 "dry_mass_g", "dry_mass", "lamina_area_mm2","xylem_area_um2")
# traits_pca <- trait_means[,!(names(trait_means) %in% droptraits)]



#no diplazium attirenses due to extra muclieage
#3 other missing xylem areas that are coming soon
#2-3 missing for stomatal size

#2 new dataframes with either data or id variables
fern_traitsonly <- trait_means[, -c(1:5)]
fern_id <- trait_means[, c(1:5)]
  ##cols
  fern_id$symbcols <- ifelse(fern_id$niche2 == "terrestrial", trtcols2[1],
                             "red")
  fern_id$symbcols <- ifelse(fern_id$niche2 == "epiphyte", trtcols2[3],
                             fern_id$symbcols)
  fern_id$symbcols <- ifelse(fern_id$niche2 == "hemi-epiphyte", trtcols2[2],
                             fern_id$symbcols)


# traits_pca_nona <- fern_traitsonly[complete.cases(fern_traitsonly),]
# fern_id2 <- fern_id[complete.cases(fern_id),]

#principle compoent analysis with scales variances
# fern_rda<- rda(traits_pca_nona,scale=T)
fern_rda2<- rda(fern_traitsonly,scale=T)

len <- .8 #length of arrow scaling
traitnames <- c("LA", "FL", "SL", "LL", "CHL", "XA", "HV", "SS", "SD", "OM",
                "TLP","E", "CAP", "SLA")

sites <- scores(fern_rda2, display='sites')
spp <- scores(fern_rda2, display='species')

jpeg(filename = "output/pca.jpeg",
     width = 7, height = 7, units = "in", res= 400)  

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
plot(sites,ylab="PC 2 (24.3 %)", xlab="PC 1 (29.9%)",type='n',
     xlim=c(-1.5, 2), ylim=c(-2, 2.25))
abline(v=0, lty='dashed')
abline(h=0, lty='dashed')
ordihull(fern_rda2, groups = fern_id$niche2, lwd=2,draw='polygon',
         col=trtcols,alpha=50, border = trtcols)
arrows(0, 0, len * spp[, 1],  len * spp[, 2], length = 0.05, lwd=1.5)
points(sites,cex=1.75, bg=fern_id$symbcols, pch=21)
text(spp,labels=traitnames,cex=1)
legend("topleft", legend= boxlabs,
       pch=21, inset=0.01, bty='n', cex=1,pt.cex=1.25, pt.bg=trtcols)

dev.off()
