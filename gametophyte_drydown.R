
### gametophyte dry downs

#1st trial with hemisphytic fern (lomariopsis)


drydown <- read.csv("raw_data/gam_drydown_trial.csv")



# data formatting ---------------------------------------------------------
drydown$date <- as.Date(drydown$date, format = "%d/%m/%Y")

gam_mass <- with(drydown, total_mass-filter_mass)

cols <- c("forestgreen", "cornflowerblue", "gold", "red")

windows()
boxplot(gam_mass ~ salt, data = drydown, col=cols)
