#format La Selva PV curve data
pv <- read.csv("raw_data/pv_curves.csv", stringsAsFactors = FALSE)
  pv$date <- as.Date(pv$date, format = "%d/%m/%Y", tz = "UTC")
  
#we need to delete phlebodium aurem and 1 serpocaulon because we didnt have inital mass
#these have a qc report as bad so this is easy (maybe can back calculate this later?)
pv_clean <- pv[!pv$qc == "bad",]

##need to seperate intial fresh mass
startmass <- pv_clean[pv_clean$psi_bars == "init",]
#save the startmas and add the pv curve data to it
curves <- pv_clean[!pv_clean$psi_bars == "init",]
  curves$psi_bars <- as.numeric(curves$psi_bars)


# dry mass of pv curve pinnae ---------------------------------------------
  #read in drymass values
  drymass <- read.csv("raw_data/fern_dryweights2.csv")

  #extract only pv data
  pv_dry <- drymass[, 1:6]
    pv_dry$date <- as.Date(pv_dry$date, format = "%d/%m/%Y", tz = "UTC")


# calculate relative water content ----------------------------------------
  rwc <- merge(startmass, pv_dry, all=TRUE)
    #drop the phelboium and a few other indivual curves where pinnae were broken
    rwc_nona <-   rwc[complete.cases(rwc),]
      rwc_nona$leafwater_t0 <- with(rwc_nona, wet_weight_g - pv_dry)
    
    rwc_final <- rwc_nona[,c(1:5,9:10)]

#need to merge leafwater_t0 and drymass to pvcurve data set
pv_calc <- merge(curves, rwc_final)

  pv_calc$RWC <- with(pv_calc, (wet_weight_g - pv_dry)/leafwater_t0)
  
  
test_curve <- pv_calc[pv_calc$species=="nephrolepis_rivularis" & pv_calc$plant_no =="4",]

  
# plotting ----------------------------------------------------------------
#1/psi vs rwc (is the goal)

plot(1/psi_bars ~ RWC, data=test_curve, xlim=c(1, .9))

#fit nls 

library(nlme)
nlsfits <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | volume,
                   start=list(g1=8),data=cond_agg2)

#model
nls_lob <- nls(height~ chapm(age, Asym, b, c), 
               data=Loblolly,
               start=list(Asym=100, b=.1, c=2.5))

library(nlstools)
overview(nls_lob)

#find a curve that is close (exponetial decay curve???, and insert parameters into nls)
chapm <- function(x, Asym, b,c) Asym*(1-exp(-b*x))^c

curve(chapm(x, Asym=100, b=.15, c=3), from =0, to=50)
curve(chapm(x, Asym=100, b=.05, c=3), add=TRUE)

