#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 11
# CH11B Are Australian weather forecasts well calibrated?
# using the australia-weather-forecasts dataset
# version 0.9 2020-09-08
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Import libraries



# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

data_in <- paste(data_dir,"australia-weather-forecasts","clean/", sep = "/")
use_case_dir <- "ch11-australia-rainfall-predict/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------


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



