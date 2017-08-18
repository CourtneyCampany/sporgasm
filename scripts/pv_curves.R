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
  pv_calc$wp_recip <- with(pv_calc, 1/psi_bars)

  
# plotting ----------------------------------------------------------------
#1/psi vs rwc (is the goal)
test_curve <- pv_calc[pv_calc$species=="nephrolepis_rivularis" & pv_calc$plant_no =="4",]
# write.csv(test_curve, "calculated_data/test_curve.csv", row.names = FALSE)
  
  plot(wp_recip ~ RWC, data=test_curve, xlim=c(1, .9))
  #get start value for nls as the slope of the logarimitic curve
  plot(log(wp_recip) ~ log(RWC), data=test_curve, xlim=c(0, -.09))
  fit <- lm(log(wp_recip) ~ log(RWC), data=test_curve)
  #use log linear model to extract a starting value for nls
  fit$coefficients #can plot the fit but it wont be great

# Fit using nls
fit_nls = nls(wp_recip ~ (RWC ^ b), start = c(b = 33), trace = T, data=test_curve)
  # The coefficient is much closer to the known
  coef(fit_nls) #b=49
  summary(fit_nls)
  pred <- predict(fit_nls)
  
# Plot of data and two estimates
  test_curve$minus <-  test_curve$RWC -1
  
  ###guess the inflection = .96
  flat_line <- test_curve[test_curve$RWC < .95,]
  fit_flat <- lm(wp_recip~RWC, data=flat_line)
  coef(fit_flat)
  #normalize to zero
  
windows()
plot(wp_recip ~ RWC, data=test_curve,xlim=c(1,.85), ylim=c(0, .99))
lines(test_curve$RWC, test_curve$RWC^coef(fit_nls), col = "red", xlim=c(1,.85))
abline(fit_top, col="blue")
abline(h=0)
abline(v=.86)

#fit the transformed data (1-rwc, to plot on negative axis to get y intercept)
fit_flat_trans <- lm(wp_recip~minus, data=flat_line)
windows()
par(mar=c(4,4,1,1))
plot(wp_recip ~ minus, data=test_curve, xlim=c(-.1, 0), ylim=c(-.1, .5), xlab="1-RWC")
abline(fit_top_trans, col="blue")

coef(fit_top_trans)
#x and y intercepts of untransformed RWC flat line
y_int <- coef(fit_flat_trans)[1] 
x_int <- (coef(fit_flat_trans)[2]-y_int)/coef(fit_flat_trans)[2]

#x_int is based on y=mx+b from normailized lm, so x_trans = x_org-1
# y= m(x_trans) + B, 
#m and B are from transfromed model

###should get the same answer for yaxis if x=100 (RW)



#function that includes an asymptotic slope--------------------------- 
#with a curvy part for the first line

#ex from Remko
pv_func <- function(t, slope, k, e0)1 - slope*t + e0*exp(-k*t)
curve(pv_func(x, slope=0.02, k=2, e0=1.4), from=0, to=10, ylim=c(0,5), xlim=c(0,20))

#my data range is x=.85,1, and y=0,1
curve(pv_func(x, slope=0.02, k=2, e0=1.4), from=.9, to=1,ylim=c(1,1.5), xlim=c(.85,1.1))
#so parameters of function need to change

pv_func2 <- function(t, slope, k, e0) 1-slope*t + e0*exp(-k*t)
curve(pv_func2(x, slope=1, k=-50.6, e0=.0000000000000000000001), 
      from=.9, to=1,xlim=c(.8,1.05))
points(wp_recip ~ RWC, data=test_curve)

#fit bew nls for the new equation
test_nls = nls(wp_recip ~ 1-slope*RWC + e0*exp(-k*RWC), 
               start = c(slope=1,e0=.0000000000000000000001,k=-50.6), 
               trace = T, 
               data=test_curve)
# The coefficient is much closer to the known
coef(test_nls)
summary(test_nls)
pred <- predict(test_nls)

plot(wp_recip ~ RWC, data=test_curve, xlim=c(.9,1), ylim=c(0,1))
lines(test_curve$RWC, test_curve$RWC^coef(test_nls), col = "red")

#inflection
derv <- D(D(1-.02*t + 1.4*exp(-2*t), 't'))

#could fit segmented regression----------------------------------------
library(segmented)

# have to provide estimates for breakpoints.
# after looking a the data, 
my.seg <- segmented(my.lm, 
                    seg.Z = ~ DistanceMeters, 
                    psi = list(DistanceMeters = c(4, 15)))

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
#my.seg <- segmented(my.lm, 
#                    seg.Z = ~ DistanceMeters, 
#                    psi = NA)

# display the summary
summary(my.seg)
# get the breakpoints
my.seg$psi

# get the slopes
slope(my.seg)


