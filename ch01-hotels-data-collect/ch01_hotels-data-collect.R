################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 02
# CH02B Identifying successful football managers

# football dataset
# version 0.9 2020-08-28

###########

# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
#----------------------------------------------------------------------------------------------------


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"hotels-vienna","clean/", sep = "/")

use_case_dir <- "ch01-hotels-data-collect/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



# load in clean and tidy data and create workfile
df <-  read.csv(paste0(data_in,"hotels-vienna.csv"))

############################################
# First look
############################################
df <- df%>%
  select(hotel_id, accommodation_type, country, city, city_actual, neighbourhood, center1label, distance,
          center2label, distance_alter, stars, rating, rating_count, ratingta, ratingta_count, year, month,
          weekend, holiday, nnights, price, scarce_room, offer, offer_cat)

summary(df)

# export list
df <- subset(df, select = c(hotel_id, accommodation_type, country, city, city_actual, center1label, distance, stars, rating, price))
write.csv(df[1:5,], paste0(output, "hotel_listobs.csv"), row.names = F)

