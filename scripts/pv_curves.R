#format La Selva PV curve data

pv <- read.csv("raw_data/pv_curves.csv")

##need to seperate intial fresh mass

startmass <- pv[pv$psi_bars == "init",]

curves <- pv[!pv$psi_bars == "init",]

#read in drymass values

drymass <- read.csv("raw_data/fern_dryweights.csv")
#extract only pv data
pv_dry <- drymass[, 1:6]


#1/psi vs rwc (is the goal)