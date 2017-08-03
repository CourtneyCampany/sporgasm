#format La Selva PV curve data
pv <- read.csv("raw_data/pv_curves.csv")
  pv$date <- as.Date(pv$date, format = "%d/%m/%Y", tz = "UTC")

##need to seperate intial fresh mass
startmass <- pv[pv$psi_bars == "init",]
#save the startmas and add the pv curve data to it
curves <- pv[!pv$psi_bars == "init",]

#read in drymass values
drymass <- read.csv("raw_data/fern_dryweights2.csv")

#extract only pv data
pv_dry <- drymass[, 1:6]
  pv_dry$date <- as.Date(pv_dry$date, format = "%d/%m/%Y", tz = "UTC")



# calculate relative water content ----------------------------------------
rwc <- merge(startmass, pv_dry, all=TRUE)

leafwater <- freshmass - drymass(one value)

rwc <- leafwater/
  
  RWC (%) = [(mass_Ti-drymass) / (freshmass_T0-drymass)] x 100


rwc <- leafwater_Ti/leafwater_T0
  
  
  
# plotting ----------------------------------------------------------------
#1/psi vs rwc (is the goal)
