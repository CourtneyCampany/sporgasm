source("master_scripts/plot_objects.R")

wp <- read.csv("calculated_data/waterpotential_middday.csv")

# windows()

# jpeg(filename = "output/n15.jpeg",
#      width = 7, height = 7, units = "in", res= 400)

par(mgp=c(2.5,1,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot((water_potential *-1)~ niche2, xaxt='n',varwidth=TRUE,data=wp,
        ylab=wp_lab, outline=FALSE, ylim=c(-1.5,0),border=trtcols,
        boxlwd=2,whisklwd=2,staplelwd=2, xlab="")
axis(1, boxlabs, at=1:3, cex=1.1)
stripchart((water_potential *-1)~ niche2, data = wp,
           vertical = TRUE, method = "jitter",
           pch = 16, col= trtcols2, xaxt='n', add=TRUE) 

# dev.off()
