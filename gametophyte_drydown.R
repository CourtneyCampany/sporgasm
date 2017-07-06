
### gametophyte dry downs

#1st trial with hemisphytic fern (lomariopsis)

drydown <- read.csv("raw_data/gam_drydown_trial.csv")


# data formatting ---------------------------------------------------------
drydown$date_t0 <- as.Date(drydown$date_t0, format = "%d/%m/%Y")
drydown$date_t1 <- as.Date(drydown$date_t1, format = "%d/%m/%Y")
drydown$id <- as.factor(drydown$id)

gam_mass_init <- with(drydown, total_mass_t0-filter_mass)
gam_mass_t1 <- with(drydown, total_mass_t1-filter_mass)

# plot objects ------------------------------------------------------------
chems <- c("Control", "KNo3", "MgSo4", "NaCl")
cols <- c("forestgreen", "cornflowerblue", "gold", "firebrick4")


# plotting ----------------------------------------------------------------

###percent mass loss
windows()
boxplot((gam_mass_init-gam_mass_t1/gam_mass_init)*100 ~ salt, data = drydown, 
        col=cols, ylab="Mass Loss (%)", outline=F,names=chems)


mean(drydown$fvfm)
