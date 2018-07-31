
#First read in the vcurve parameters csv (with times)------------
vcurve_params <- read.csv("somefilepath.csv")

#Next read in and format all the raw flow files----------------

## Here is code to read in all files into a list (a list of dataframes)
## this is done by matching a pattern
##the path will be whatever folders we set up in the project... 
##(i.e. vcurves/hydraulics_raw)
##pattern will be something each filename shares (MPa)
##..lets correct these manually


#following code creates a list of clean file names that we will use latter
vcurves<- list.files(path="vcurves/hydraulics_raw/",pattern="MPa",full.names=TRUE)

#now simplify the name of each file
#remove all unnecessary info and keep the genusspecies and treatment info
##code may need to be modified
vcurves_names <- gsub("vcurves/hydraulics_raw/", "", vcurves)
vcurves_names <- gsub(".csv", "", vcurves_names)
##this will generate a unique set of names without the junk (hopefully)
## i.e. genusspecies2_1.5mpa


#now we will read in all the files using the plyr package (install it if needed)
library(plyr)

#this isnt complete because we need to add the arguments to read.csv
# to remove the first 14 rows...skip=14
# should be able to add skip=14 to the last set of paranthesis 
#which perform read.csv()
vcurve_files <- llply(list.files(path="vcurves/hydraulics_raw/",
            pattern="MPa",full.names=TRUE),function(filename) {
            dat=read.csv(filename, header=TRUE)
})

#i think this is how we assign the simplified names to each csv in the list
#i need to test this first
setNames(vcurve_files, vcurves_names)

#test to see if read.csv worked bu viewing one element of the list
vcurve_files[5]


#Build function------------

#best way to test will be to load the first raw data file
#and isolate the matching row from the params dataframe
#then run them through whatever function

test <- read.csv("vcurves/hydraulics_raw/firstfile.csv", skip=14)
test_params <= vcurve_params[logicalargumenttoisolateonerow , ]

testfunction <- function(dfr1, paramsdfr,) {
  paramsdfr$intialtime <= time1
  #add others
  
  initialback <- dfr1[dfr1$relatimes >= time1 & dfr1$relatimes <= time2,]
  flow <- dfr1[dfr1$relatimes >= time3 & dfr1$relatimes <= time4,]
  finalback <- dfr1[dfr1$relatimes >= time5 & dfr1$relatimes <= time6,]
  
  #borrow code below for quanitiles and cleaning
  
  #then calculate stuff and make means and of intial, flow, final
  
  #then merge 3 flows backtogether
  #google pylr package multiple merges..or...
  
  finaldat <- merge(intialback, flow)
  finaldat2 <- merge(finaldat, finalback)
  #once merged return finished product
  return(finaldat2)
  
}

##see if it works ....
totest <- testfunction(test, testparams)

#Here is the base elements for function clean the flow data------------

test <- read.csv("06July2018_SFrax2_1.0MPa.csv", skip=14)
  plot(Flow..ul.min. ~ Sample.., data=test)

flow <- test[test$Sample.. >= 465 & test$Sample.. <= 565,]
  plot(Flow..ul.min. ~ Sample.., data=flow)


flow_format_function <- function(x){
  
  upper_quant <- quantile(x$Flow..ul.min, prob = .90)
  lower_quant <- quantile(x$Flow..ul.min, prob = .10)
  
  clean_flow_rate <- x[x$Flow..ul.min >= lower_quant & 
                       x$Flow..ul.min <= upper_quant,]
  return(x)
}

testfunc <- flow_format_function(flow)

windows()
par(mfrow=c(2,1), mar=c(5,1,1,1))
plot(Flow..ul.min. ~ Sample.., data=flow, ylim=c(2,5))
plot(Flow..ul.min. ~ Sample.., data=testfunc, ylim=c(2,5))



#at some point we can run the list of files through the function---------

flow_format <- llply(vcurve_files, function(x) flow_format_function(x))