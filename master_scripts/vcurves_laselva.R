## laselva vulnerability curve format


## extract and average backups (x2) and flow rates

laselva_times <- read.csv("raw_data/vcurve_raw_data/laselva/vcurve_times_laselva.csv")


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

setNames(vcurve_files, vcurves_names) #set dataframe names to genusspecies & MPa

#turn times into a list, double check that each list element aligns with data
times_list <- split(laselva_times, seq(nrow(laselva_times)))

vcurve_function <- function(dfr, timesdfr, 
                            massflow_constant1 = -0.2323,
                            massflow_constant2 = 1002.9) {
    x <- dfr
    y <- timesdfr
    
    y$h20_dens <- (massflow_constant1 * y$air_temp_C ) + massflow_constant2
  
  #isolate backgrounds and flow from times dataframe 
  # background1 <- x[x$Relative.Time.s. >= y$back_first_initial
  #                     & x$Relative.Time.s. <= y$back_first_final ,]
  
  # background2 <- x[x$Relative.Time.s. >= y$back_second_initial
  #                     & x$Relative.Time.s. <= y$back_second_final ,]
  # 
  # flow <- x[x$Relative.Time.s. >= y$flow_initial
  #              & x$Relative.Time.s. <= y$flow_final ,]
  # 
  # #mass flow unit conversion
  # background1$background1_mgsec <- (((background1$Flow..ul.min. * 10^-9)
  #                                    *times$h20_dens)*1000000)/60
  # 
  # 
  # background2$background2_mgsec <- (((background2$Flow..ul.min. * 10^-9)
  #                                    * times$h20_dens)*1000000)/60
  # 
  # flow$flow_mgsec <- (((flow$Flow..ul.min. * 10^-9)
  #                      * times$h20_dens)*1000000)/60
  # 
  # ##average mass flow rates or each data set
  # background1_mean <- mean(background1$background1_mgsec)
  # background2_mean <- mean(background2$background2_mgsec)
  # flow_mean <- mean(flow$flow_mgsec)
  # 
  # #average backgrounds
  # bakground_agg <- (background1_mean + background2_mean)/2
  # 
  # flow_params <- dataframe(flow = flow_mean, background = background_agg)
   return(y)
}


#test function 

testdata <- vcurve_files[1]
testtimes <- times_list[1]

test <- vcurve_function(dfr = testdata, timesdfr = testtimes)
