## laselva vulnerability curve format


## extract and average backups (x2) and flow rates

laselva_times <- read.csv("raw_data/vcurve_raw_data/laselva/vcurve_times_laselva.csv")
  laselva_times$water_potential <- round(laselva_times$water_potential,1)
  laselva_times$SampleID <- with(laselva_times,
                            paste(species, individual, 
                            format(water_potential,digits=1), sep="_"))
  timesnames <- laselva_times$SampleID
  
#turn times into a list, double check that each list element aligns with data
  times_list <- split(laselva_times, seq(nrow(laselva_times)))
  # times_list2 <- setNames(times_list, timesnames)

##read in all data:

#create a list of clean file names that we will use latter
vcurves <- list.files(path="raw_data/vcurve_raw_data/laselva/",
                      pattern="mpa",full.names=TRUE)
#keep the genusspecies and treatment info

vcurves_names <- gsub("raw_data/vcurve_raw_data/laselva/", "", vcurves)
vcurves_names <- gsub("_mpa.csv", "", vcurves_names)

##read in all data files from laselva

library(plyr)

#skip first 14 rows from sensirion flow meter csv files
vcurve_files <- llply(list.files(path="raw_data/vcurve_raw_data/laselva/",
                                 pattern="mpa",full.names=TRUE),function(filename){
                                 dat=read.csv(filename, header=TRUE,skip=14)
                                 })

# vcurve_files2 <- setNames(vcurve_files, vcurves_names) 
#set dataframe names to genusspecies & MPa

#add new variable with unique ID
for( i in seq_along(vcurve_files)){
  
  vcurve_files[[i]]$SampleID <- vcurves_names[i]
  
}

## function to calculate vulnerability curves
vcurve_function <- function(dfr, timesdfr,
                            massflow_constant1 = -0.2323,
                            massflow_constant2 = 1002.9,
                            acceleratebygrav = 0.09806) {
  
    x <- data.frame(dfr[[1]])
    y_alltimes <- timesdfr
    # y_alltimes <- data.frame(timesdfr[[1]])

    #subset times dataframe to match sample id of individual curve
    y <- y_alltimes[y_alltimes$SampleID == unique(x$SampleID),]

    y$h20_dens <- (massflow_constant1 * y$air_temp_C )
                  + massflow_constant2

  # #isolate backgrounds and flow from times dataframe to trim flow data sets
    background1 <- x[x$Relative.Time.s. >= y$back_first_initial
                       & x$Relative.Time.s. <= y$back_first_final ,]
    
  background2 <- x[x$Relative.Time.s. >= y$back_second_initial
                       & x$Relative.Time.s. <= y$back_second_final ,]

  flow <- x[x$Relative.Time.s. >= y$flow_initial
                & x$Relative.Time.s. <= y$flow_final ,]

  #mass flow unit conversion
  background1$background1_mgsec <- (((background1$Flow..ul.min. * 10^-9)
                                     *y$h20_dens)*1000000)/60


  background2$background2_mgsec <- (((background2$Flow..ul.min. * 10^-9)
                                     * y$h20_dens)*1000000)/60

  flow$flow_mgsec <- (((flow$Flow..ul.min. * 10^-9)
                       * y$h20_dens)*1000000)/60

  ##average mass flow rates or each data set
   background1_mean <- mean(background1$background1_mgsec)
  background2_mean <- mean(background2$background2_mgsec)
  flow_mean <- mean(flow$flow_mgsec)

  #average backgrounds
  background_agg <- (background1_mean + background2_mean)/2

  flow_params <- data.frame(flow = flow_mean, background = background_agg)

  #calculate corrected mass flow rate (mg sec)
  mass_flow_rate <- flow_mean - background_agg

  #convert to conductivity
  pressurehead <- y$pressure_head_cm * acceleratebygrav

  conductivity <- mass_flow_rate/(pressurehead/y$stipe_length_mm) #stipe must be mm
  #mg mm KPa-1s-1

  return(background1_mean)
}

#test function 

test <- lapply(vcurve_files, function(x) 
         vcurve_function(x, laselva_times))

test2 <- llply(vcurve_files, function(x)  
                vcurve_function(x, laselva_times))

testdata <- vcurve_files[1]
testtimes <- times_list[1]

test <- vcurve_function(dfr = testdata, timesdfr = laselva_times)



tester <- mapply(dfr = vcurve_files, timesdfr = times_list,
                 FUN = function(dfr, timesdfr) vcurve_function(dfr, timesdfr))

tester <- mapply(vcurve_function, vcurve_files,  times_list)

test <- mapply(function(x,y) vcurve_function(x,y), vcurve_files,  times_list)
