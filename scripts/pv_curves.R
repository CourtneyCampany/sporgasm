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
  

  
# plotting ----------------------------------------------------------------
#1/psi vs rwc (is the goal)
test_curve <- pv_calc[pv_calc$species=="nephrolepis_rivularis" & pv_calc$plant_no =="4",]
  test_curve$wp_recip <- with(test_curve, 1/psi_bars)

  plot(wp_recip ~ RWC, data=test_curve, xlim=c(1, .9))
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
  
windows()
plot(wp_recip ~ RWC, data=test_curve,xlim=c(1,.85), ylim=c(-.4, .99))
lines(test_curve$RWC, test_curve$RWC^coef(fit_nls), col = "red", xlim=c(1,.85))
abline(fit_top, col="blue")
abline(h=0)
abline(v=.86)

###guess the inflection = .96
top_line <- test_curve[test_curve$RWC < .95,]
fit_top <- lm(wp_recip~RWC, data=top_line)
coef(fit_top)
#normalize to zero

fit_top_trans <- lm(wp_recip~minus, data=top_line)

windows()
plot(wp_recip ~ minus, data=test_curve, xlim=c(-.1, 0), ylim=c(-.1, .5))
abline(fit_top_trans, col="blue")

coef(fit_top_trans)
#x and y intercepts of untransformed data
y_int <- coef(fit_top_trans)[1] 

x_int <- (coef(fit_top_trans)[2]-y_int)/coef(fit_top_trans)[2]

#x_int is based on y=mx+b from normailized lm, so x_trans = x_org-1
# y= m(x_trans) + B, 
#m and B are from transfromed model

###should get the same answer for yaxis if x=100 (RW)


library(inflection)
knee <-edeci(x=test_curve$RWC, y=pred,0)
uik=knee[1]
uik

ede(x=test_curve$RWC, y=pred, index=0)
edeci(x=test_curve$RWC, y=test_curve$wp_recip, index=1, k = 5)
findiplist(x=test_curve$RWC, y=pred, index=0)
# #use this when automating with nlsfits
# form <- wp_recip ~ R(a,b,RWC)
# fit <- nls(form, data=test_curve, start=list(a=1,b=0.01))








