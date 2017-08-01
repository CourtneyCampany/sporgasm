# standard error function
se <- function(x) sd(x)/sqrt(length(x))

# functions for sporgasm project ------------------------------------------

###Here are the various function used in the sporsgasm project

x$rwc <- with(x, fresh_mass- dry_mass / saturation_mass - dry_mass)



#1. fit a nonliner curve to all data

#2. determine turgor loss point as the inflection point if the curve

fitcurve <- nls(dat)
plot(fitcuve)
#if the curv fits then run the inflection package
library(inflection)
inflection_pt <- edeci(y= 1/dat$wp, x=dat$rwc)

#3. build a linear regresssion with the turgor loss point and the rwc > turgor loss point


#4. fit the linear regression from the turgor loss point, wp_sat is the x axis (do the math)
coef(fit)[1]
#need wp_sat

#5. bulk_modulus

 
# pressure volume curves --------------------------------------------------

pv_curve_func <- function(x){
  
  
}


pv_plot_func <- function(x){
 
  
  plot(1/ x$wp ~ rwc)
}