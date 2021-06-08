# load packages
library(dplyr)
library(stringr)

## read in datasheet with flow parameters

lascruces_times <- 
  read.csv("raw_data/vcurve_raw_data/lascruces/vcurve_times_lascruces_ab.csv") %>% 
         mutate(water_potential = format(water_potential, nsmall=2),
         sample_id = paste(species_id, individual, 
         format(water_potential,digits=2), sep="_"))


#create a list of clean file names from lascruces raw data with .csv extension
vcurves <- list.files(path="raw_data/vcurve_raw_data/lascruces/",
                      pattern="mpa",
                      full.names=TRUE)

#extract the genusspecies and treatment info
vcurves_names <- str_replace(vcurves, "raw_data/vcurve_raw_data/lascruces/", "") %>%
  str_replace("_mpa.csv", "")

#read in all data files from lascruves using file path from vcurves object (above)
#skip first 14 rows from sensirion flow meter csv files
vcurve_data_list <- lapply(vcurves, read.csv, header=TRUE, skip=14)


#add new variable to each list object with matching unique ID 
for(i in seq_along(vcurve_data_list)){
  vcurve_data_list[[i]]$sample_id <- vcurves_names[i]
}


## function to calculate conductivity from stipe flow rates--------
vcurve_function <- function(dfr, timesdfr,
                            massflow_constant1 = -0.2323,
                            massflow_constant2 = 1002.9,
                            acceleratebygrav = 0.09806) {
  
  x <- data.frame(dfr)
  
  #subset times dataframe to match sample id of an individual curve
  y <- timesdfr[timesdfr$sample_id == unique(x$sample_id),]
  
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
  
  conductivity <- corr_flow_rate/(pressurehead/y$stipe_length_mm) #stipe must be in mm
  #mg mm KPa-1s-1
  
  id_cond <- data.frame(sample_id = unique(x$sample_id), 
                        genusspecies = y$species_id, 
                        individual = y$individual,
                        K = conductivity, 
                        flow = corr_flow_rate,
                        phead = pressurehead,
                        bg = background_agg,
                        MPa = as.numeric(format(y$water_potential, nsmall=2)),
                        curve_id = paste(y$species_id, y$individual, 
                                                   sep = "-"))
  return(id_cond)
}


#test function with simple data frames----------
# testdata <- vcurve_data_list[[290]]
# test1 <- vcurve_function(dfr = testdata, timesdfr = lascruces_times)

#run function with all data ----------
lascruces_cond <- lapply(vcurve_data_list, 
                       vcurve_function,
                       timesdfr=lascruces_times) %>%
                       dplyr::bind_rows(.)



#calculate %loss conductivity, split each curve into a list
lascruces_list <-  split(lascruces_cond, as.factor(lascruces_cond$curve_id))

#quick function to calculate percent loss K
percloss_func <- function(x){
  Kdata <- data.frame(x)
  kmax <- Kdata[Kdata$MPa == 0.00, "K"] #isolate kmax for each curve
  Kdata$perc_loss_k <- 1- Kdata$K/kmax #percent loss K at each MPa
  Kdata$PLC <-  Kdata$perc_loss_k * 100
  return(Kdata)
}


lascruces_cond_loss<- lapply(lascruces_list, 
                             percloss_func) %>%
                             dplyr::bind_rows(.)

write.csv(lascruces_cond_loss, "calculated_data/lascruces_vcurves.csv", 
          row.names = FALSE)

#lets save a dataset that is only Kmax
lascruces_kmax <- lascruces_cond[lascruces_cond$MPa == 0.00,]
write.csv(lascruces_kmax, "calculated_data/lascruces_kmax.csv", 
          row.names = FALSE)
