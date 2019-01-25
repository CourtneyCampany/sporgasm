## laselva vulnerability curve format
library(plyr)

## extract and average backups (x2) and flow rates

laselva_times <- read.csv("raw_data/vcurve_raw_data/laselva/vcurve_times_laselva.csv")
  laselva_times$water_potential <- round(laselva_times$water_potential,1)
  laselva_times$SampleID <- with(laselva_times,
                            paste(species, individual, 
                            format(water_potential,digits=1), sep="_"))
  # timesnames <- laselva_times$SampleID
  
#turn times into a list, double check that each list element aligns with data
  # times_list <- split(laselva_times, seq(nrow(laselva_times)))
  # times_list2 <- setNames(times_list, timesnames)

##Read in flow meter data files

#create a list of clean file names that we will use latter
vcurves <- list.files(path="raw_data/vcurve_raw_data/laselva/",
                      pattern="cycsem_1",full.names=TRUE)

  #extract the genusspecies and treatment info
  vcurves_names <- gsub("raw_data/vcurve_raw_data/laselva/", "", vcurves)
  vcurves_names <- gsub("_mpa.csv", "", vcurves_names)


##read in all data files from laselva

#skip first 14 rows from sensirion flow meter csv files
vcurve_files <- llply(list.files(path="raw_data/vcurve_raw_data/laselva/",
                                 pattern="cycsem_1",full.names=TRUE),function(filename){
                                 dat=read.csv(filename, header=TRUE,skip=14)
                                 })

# vcurve_files2 <- setNames(vcurve_files, vcurves_names) 
#set dataframe names to genusspecies & MPa

#add new variable with unique ID
for(i in seq_along(vcurve_files)){
  vcurve_files[[i]]$SampleID <- vcurves_names[i]
}

## function to calculate conductivity from stem flow rates
vcurve_function <- function(dfr, timesdfr,
                            massflow_constant1 = -0.2323,
                            massflow_constant2 = 1002.9,
                            acceleratebygrav = 0.09806) {

    x <- data.frame(dfr)

    #subset times dataframe to match sample id of an individual curve
    y <- timesdfr[timesdfr$SampleID == unique(x$SampleID),]
    
    #density of H20 calculation
    y$h20_dens <- (massflow_constant1 * y$air_temp_C )+ massflow_constant2

    #isolate backgrounds (x2) and flow from timesdfr to trim flow data sets
    #ackward variable names are raw names from flow meter
    background1 <- dfr[dfr$Relative.Time.s. > (y$back_first_initial-1)
                       & dfr$Relative.Time.s. < y$back_first_final ,]

    background2 <- x[x$Relative.Time.s. > (y$back_second_initial-1)
                       & x$Relative.Time.s. < y$back_second_final ,]

    flow <- x[x$Relative.Time.s. > (y$flow_initial-1)
                & x$Relative.Time.s. < y$flow_final ,]

    #mass flow unit conversion for each set of flow data
    background1$background1_mgsec <- (((background1$Flow..ul.min. * 10^-9)
                                     * y$h20_dens) * 1000000)/60


    background2$background2_mgsec <- (((background2$Flow..ul.min. * 10^-9)
                                     * y$h20_dens) * 1000000)/60

    flow$flow_mgsec <- (((flow$Flow..ul.min. * 10^-9)
                       * y$h20_dens) * 1000000)/60

    ##average mass flow rates or each data set
    background1_mean <- mean(background1$background1_mgsec)
    background2_mean <- mean(background2$background2_mgsec)
    flow_mean <- mean(flow$flow_mgsec)

    #average backgrounds
    background_agg <- (background1_mean + background2_mean)/2

    #calculate corrected mass flow rate (mg sec-1)
    corr_flow_rate <- flow_mean - background_agg

    #convert to conductivity
    pressurehead <- y$pressure_head_cm * acceleratebygrav

    conductivity <- corr_flow_rate/(pressurehead/y$stipe_length_mm) #stipe must be mm
    #mg mm KPa-1s-1

    return(conductivity)
}

#test function with simple data frames
# testdata <- vcurve_files[[2]]
# test1 <- vcurve_function(dfr = testdata, timesdfr = laselva_times)

#test function with larger list
test3 <- lapply(vcurve_files, function(x) 
          vcurve_function(x,  timesdfr=laselva_times))

# 
# for(i in 1:length(vcurve_files)){
#   x <- data.frame(vcurve_files[[i]])
#   vcurve_function(dfr=x,  timesdfr=laselva_times)
# }

