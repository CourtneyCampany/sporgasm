source("master_scripts/plot_objects.R")


#huber by niche
huber <- read.csv("calculated_data/xylem_area_huber.csv")


jpeg(filename = "output/huber.jpeg",
      width = 7, height = 7, units = "in", res= 400) 

par(mgp=c(2.5,.75,0), mar=c(4,4,1,1), cex.lab=1.1)
boxplot(huber ~ niche2, data=huber,xaxt='n',
        ylab = "Huber values",border=trtcols, varwidth=TRUE, outline=FALSE)
axis(1, boxlabs, at=1:3, cex.axis=1.1)
stripchart(huber ~ niche2, data = huber,
           vertical = TRUE, method = "jitter",
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

dev.off()
