#*********************************************************************
# DATA ANALYSIS TEXTBOOK
# CH 11 PROBABILITY MODELS
# weather australia

#*********************************************************************
# WHAT THIS CODES DOES:
## selects a single station, shows calibration curve


# clear environment
rm(list=ls())

source("global.R")

use_case_dir <- file.path("ch11-australia-rainfall-predict/")
loadLibraries(use_case_dir)

data_in <- paste(data_dir,"australia-weather-forecasts","clean", sep = "/")

data_out <- use_case_dir
output <- paste0(use_case_dir,"/output/")
create_output_if_doesnt_exist(output)

data <- read.csv(paste(data_in, "rainfall_australia.csv", sep = "/")) %>%
  filter(station_name=="DARWIN AIRPORT")

data <- data %>%
  filter(bd_FC_Before_Start == 39) %>%
  mutate( 
    rain_prob_fc=prob/100,
  )

# replace bin = bin+0.05

create_calibration_plot(data, 
  file_name = "ch11-figure-6-weather-calib", 
  prob_var = "rain_prob_fc", 
  actual_var = "daily_sum",
  breaks=c(0, 0.005, 0.1,0.2,0.3,0.4,0.5,0.6, 0.7, 0.8, 0.9))

Hmisc::describe(data$rain_prob_fc)



