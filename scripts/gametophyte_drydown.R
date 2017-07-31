
### gametophyte dry downs

#trials with ferns from different niches

drydown <- read.csv("raw_data/gam_drydown_trial.csv")

# data formatting ---------------------------------------------------------
  drydown$date <- as.Date(drydown$date, format = "%m/%d/%Y",tz='UTC')
  drydown$id <- as.factor(drydown$id)

  #calculate gametophyte mass
  drydown$gam_mass <- with(drydown, total_mass-filter_mass)

  #merge date and time into datetime object
  drydown$datetime <- with(drydown, paste(date, time, sep = " "))
  drydown$datetime <- as.POSIXct(drydown$datetime,tz='UTC', format="%Y-%m-%d %H:%M")
  
  #species subsets
  lomves <- drydown[drydown$species=="lomves",]
  camp <- drydown[drydown$species=="camp",]
  
  #means
  library(doBy)
  camp_agg <- summaryBy(gam_mass ~ salt + drydown_no+datetime, data=camp, 
                        FUN=mean, keep.names = TRUE)
  lomves_agg <- summaryBy(gam_mass ~ salt+datetime, data=lomves, 
                          FUN=mean, keep.names = TRUE)
  
# plot objects ------------------------------------------------------------
chems <- c("Control", "KNo3", "MgSo4", "NaCl")

library(RColorBrewer)
gradient <- colorRampPalette(c("darkgreen", "red3"))
cols <- c(gradient(8)[1],gradient(8)[3],gradient(8)[5],gradient(8)[8])


# plotting ----------------------------------------------------------------
#camp
windows()
plot(gam_mass ~ datetime, col = cols[1],subset(camp_agg, salt== "c"),lwd=2,
     type='b', ylab="Gametophyte mass (g)",xlab="",ylim=c(0, 0.015))
  lines(gam_mass ~ datetime, col = cols[2],subset(camp_agg, salt== "kno3"),lwd=2, type='b')
  lines(gam_mass ~ datetime, col = cols[3],subset(camp_agg, salt== "mgso4"),lwd=2, type='b')
  lines(gam_mass ~ datetime, col = cols[4],subset(camp_agg, salt== "nacl"),lwd=2, type='b')
  legend("topleft",chems, lty=1, col=cols, bty='n',inset=.01, lwd=2)
#lomves
windows()
plot(gam_mass ~ datetime, col = cols[1],subset(lomves_agg, salt== "c"),lwd=2,
       type='b', ylab="Gametophyte mass (g)",xlab="",ylim=c(0, 0.04))
lines(gam_mass ~ datetime, col = cols[2],subset(lomves_agg, salt== "kno3"),lwd=2, type='b')
lines(gam_mass ~ datetime, col = cols[3],subset(lomves_agg, salt== "mgso4"),lwd=2, type='b')
lines(gam_mass ~ datetime, col = cols[4],subset(lomves_agg, salt== "nacl"),lwd=2, type='b')
legend("topleft",chems, lty=1, col=cols, bty='n',inset=.01, lwd=2)
