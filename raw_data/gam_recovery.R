##gam flouresence recovery

fvfm <- read.csv("raw_data/gam_fvfm.csv")

# data formatting ---------------------------------------------------------
fvfm$date <- as.Date(fvfm$date, format = "%m/%d/%Y",tz='UTC')
fvfm$id <- as.factor(fvfm$id)
fvfm$trial_no <- as.factor(fvfm$trial_no)
fvfm$fvfm <- as.numeric(fvfm$fvfm)

#merge date and time into datetime object
fvfm$datetime <- with(fvfm, paste(date, time, sep = " "))
fvfm$datetime <- as.POSIXct(fvfm$datetime,tz='UTC', format="%Y-%m-%d %H:%M")

#species subsets
lomves <- fvfm[fvfm$species=="lomves",]
camp <- fvfm[fvfm$species=="camp",]


library(doBy)
lomves_agg <- summaryBy(fvfm ~ salt + drydown_no+datetime, data=lomves, 
                      FUN=mean, keep.names = TRUE)

library(RColorBrewer)
gradient <- colorRampPalette(c("darkgreen", "red3"))
cols <- c(gradient(8)[1],gradient(8)[1],gradient(8)[3],gradient(8)[3],
          gradient(8)[5],gradient(8)[5],gradient(8)[8],gradient(8)[8])

windows()
boxplot(fvfm ~ trial_no*salt, data=lomves, at=c(1,2,3,5,6,7,9,10,11,12,14,15), col=cols)





