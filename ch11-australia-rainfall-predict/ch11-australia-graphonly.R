#*********************************************************************
# DATA ANALYSIS TEXTBOOK
# CH 11 PROBABILITY MODELS
# weather australia

  #* v 2020-01-13 
  # v 2020-02-01
  # v 2020-03-18
# v1.1 2020-04-23 names ok

#*********************************************************************
# WHAT THIS CODES DOES:
## selects a single station, shows calibration curve


# clear environment
rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)

# Set directories ---------------------------------------------------------


# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")

data_in <- paste0(dir, "da_data_repo/australia-weather-forecasts/clean/")
data_out <- paste0(dir, "da_case_studies/ch11-australia-rainfall-predict/")
output <- paste0(dir,"da_case_studies/ch11-australia-rainfall-predict/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


# load ggplot theme function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))


# import data
setwd(data_out)
data <- read.csv("australia-bins.csv")



# visualization
calibration_plot <-  ggplot(data = data) +
  geom_line(aes(bin, rain_prob_fc), color=color[1], size=0.6, show.legend = TRUE) +
  geom_point(aes(bin,rain_prob_fc), color = color[1], size = 1, shape = 16, alpha = 0.7, show.legend=F, na.rm = TRUE) +
  geom_segment(x=0, xend=0.85, y=0, yend=0.85, color=color[2], size=0.3) +
  theme_bg() +
  labs(x="Bins of predicted probaiblities",
       y="Proportion rainy days") +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) +
  scale_x_continuous(expand=c(0.01,0.01),breaks=c(seq(0,1,0.1))) 
calibration_plot
#save_fig("ch11-weather-calib", output, "small")
save_fig("ch11-figure-6-weather-calib",output, "small")

