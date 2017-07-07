
### gametophyte dry downs

#1st trial with hemisphytic fern (lomariopsis)

drydown <- read.csv("raw_data/gam_drydown_trial.csv")


# data formatting ---------------------------------------------------------
drydown$date_t0 <- as.Date(drydown$date_t0, format = "%m/%d/%Y",tz='UTC')
drydown$date_t1 <- as.Date(drydown$date_t1, format = "%m/%d/%Y",tz='UTC')
drydown$date_t2 <- as.Date(drydown$date_t2, format = "%m/%d/%Y",tz='UTC')
drydown$date_t2 <- as.Date(drydown$date_t3, format = "%m/%d/%Y",tz='UTC')
drydown$id <- as.factor(drydown$id)

#calculate gametophyte mass
gam_mass_init <- with(drydown, total_mass_t0-filter_mass)
gam_mass_t1 <- with(drydown, total_mass_t1-filter_mass)
gam_mass_t2 <- with(drydown, total_mass_t2-filter_mass)
gam_mass_t3 <- with(drydown, total_mass_t3-filter_mass)

#merge date and time into datetime object
drydown$datetime <- with(drydown, paste(date_t0, time0, sep = " "))
drydown$datetime2 <- as.POSIXct(drydown$datetime,tz='UTC', format="%Y-%m-%d %H:%M")

# plot objects ------------------------------------------------------------
chems <- c("Control", "KNo3", "MgSo4", "NaCl")
cols <- c("forestgreen", "cornflowerblue", "gold", "firebrick4")


# plotting ----------------------------------------------------------------

lomves <- drydown[drydown$species=="lomves",]
camp <- drydown[drydown$species=="camp",]

plot()

###percent mass loss
windows()
boxplot((gam_mass_init-gam_mass_t1/gam_mass_init)*100 ~ salt, data = drydown, 
        col=cols, ylab="Mass Loss (%)", outline=F,names=chems)


mean(drydown$fvfm)
