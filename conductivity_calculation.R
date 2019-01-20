alltimes <- read.csv("vul_curve_times.csv")

times <- alltimes[alltimes$water_potential == 0.0,]

data <- read.csv("vcurve_raw_data/Cyc1_0.0_MPa.csv", skip=14)


##converting volumetric flow rate to mass flow rate
massflow_constant1 <- -.2323
massflow_constant2 <- 1002.9


times$h20_dens <- (massflow_constant1 * times$air_temp_C ) + massflow_constant2


### isolate data by times for each 60sec measurement period
background1 <- data[data$Relative.Time.s. >= times$back_first_initial
                      & data$Relative.Time.s. <= times$back_first_final ,]
##double check the exact entry exit points

background2 <- data[data$Relative.Time.s. >= times$back_second_initial
                    & data$Relative.Time.s. <= times$back_second_final ,]

flow <- data[data$Relative.Time.s. >= times$flow_initial
                    & data$Relative.Time.s. <= times$flow_final ,]


###convert to mass flow for backgrounds and flow
background1$background1_mgsec <- (((background1$Flow..ul.min. * 10^-9)
                                   *times$h20_dens)*1000000)/60


background2$background2_mgsec <- (((background2$Flow..ul.min. * 10^-9)
                                   * times$h20_dens)*1000000)/60

flow$flow_mgsec <- (((flow$Flow..ul.min. * 10^-9)
                        * times$h20_dens)*1000000)/60

##average mass flow rates or each data set
background1_mean <- mean(background1$background1_mgsec)
background2_mean <- mean(background2$background2_mgsec)
flow_mean <- mean(flow$flow_mgsec)

#average backgrounds
bakground_agg <- (background1_mean + background2_mean)/2

#calculate corrected mass flow rate (mg sec)

mass_flow_rate <- flow_mean - bakground_agg

#conver to conductivity
acceleratebygrav <- 0.09806

pressurehead <- times$pressure_head_cm * acceleratebygrav

conductivity <- mass_flow_rate/(pressurehead/times$stipe_length_mm) #stipe must be mm
#mg mm KPa-1s-1

######
#1) calculate all conductivity for each species + individual
#2) plot vulnerabilty curve

#3) calculate % loss conductivity (PLC) as % loss from max

##Use pamenterwillingen fit for PLC 12, 50, 88%

#4) calculate xylem specific max flow rates (with Alex data)
#5) calculate leaf specific max flow rates


## VISUALIZE AND RUN WITH FITplc



plc_func <- function(times, data, ...) {
  times$
  
}

