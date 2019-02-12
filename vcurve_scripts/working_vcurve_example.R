#vcurve individual tests for bad data

# laselva_times <- 
#   read.csv("raw_data/vcurve_raw_data/laselva/vcurve_times_laselva.csv") %>%  
#   mutate(water_potential = format(water_potential, nsmall=2),
#          sample_id = paste(species, individual, 
#          format(water_potential,digits=2), sep="_"))

lascruces_times <- 
  read.csv("raw_data/vcurve_raw_data/lascruces/vcurve_times_lascruces.csv") %>% 
  mutate(water_potential = format(water_potential, nsmall=2),
  sample_id = paste(species_id, individual, 
  format(water_potential,digits=1), sep="_"))


##Read in flow meter data files

#create a list of clean file names that we will use latter
vcurves <- list.files(path="raw_data/vcurve_raw_data/lascruces/",
                      pattern="parexc_10_1.5",full.names=TRUE)

#extract the genusspecies and treatment info
vcurves_names <- str_replace(vcurves, "raw_data/vcurve_raw_data/lascruces/", "") %>%
  str_replace("_mpa.csv", "")

#read in all data files from laselva using file path from vcurves object (above)
#skip first 14 rows from sensirion flow meter csv files
vcurve_data_list <- lapply(vcurves, read.csv, header=TRUE, skip=14)


#add new variable to each list object with matching unique ID 
for(i in seq_along(vcurve_data_list)){
  vcurve_data_list[[i]]$sample_id <- vcurves_names[i]
}

## calculate conductivity
times <- lascruces_times

massflow_constant1 = -0.2323
massflow_constant2 = 1002.9
acceleratebygrav = 0.09806
  

x <- data.frame(vcurve_data_list[[1]])


#subset times dataframe to match sample id of an individual curve
y <- times[times$sample_id == unique(x$sample_id),]

#density of H20 calculation
y$h20_dens <- (massflow_constant1 * y$air_temp_C )+ massflow_constant2
  
#isolate backgrounds (x2) and flow from timesdfr to trim flow data sets
background1 <- x[x$Relative.Time.s. > (y$back_first_initial-1)
                   & x$Relative.Time.s. < (y$back_first_final) ,]
  
background2 <- x[x$Relative.Time.s. > (y$back_second_initial-1)
                   & x$Relative.Time.s. < y$back_second_final ,]
  
flow <- x[x$Relative.Time.s. > (y$flow_initial-1)
            & x$Relative.Time.s. < y$flow_final ,]
  
#mass flow unit conversion for each set of flow data (mg/sec)
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
  background_agg <- mean(c(background1_mean,background2_mean))
  
  #calculate corrected volume flow rate (mg sec)
  corr_flow_rate <- flow_mean - background_agg
  
  #convert to conductivity
  pressurehead <- y$pressure_head_cm * acceleratebygrav #kPa
  
  conductivity <- corr_flow_rate/(pressurehead/y$stipe_length_mm) #stipe must be mm
  #mg mm KPa-1s-1
  