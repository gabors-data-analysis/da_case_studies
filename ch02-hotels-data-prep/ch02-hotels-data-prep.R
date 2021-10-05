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

# CH02A Finding a good deal among hotels: data preparation
# using the hotels-vienna dataset
# version 0.9 2020-09-06

###########


# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(haven)
library(Hmisc)
library(desc)
library(reshape2)
library(modelsummary)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in_clean <- paste(data_dir,"hotels-vienna","clean/", sep = "/")
data_in_raw <- paste(data_dir,"hotels-vienna","raw", sep = "/")

use_case_dir <- "ch02-hotels-data-prep/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# load in clean and tidy data and create workfile
data <- read_csv(paste0(data_in_clean,"/hotels-vienna.csv",sep=""))
# Can load from website as well
# data <- read_csv("https://osf.io/y6jvb/download")
data <- data %>% select(hotel_id, accommodation_type ,distance, stars,rating,rating_count,price)

# look at accomodation types
table(data$accommodation_type)

#**********************************************
#     Table 1.1
#**********************************************

head(data,n=5)


#**********************************************
#     Table 2.2
#**********************************************
data[2,]

#**********************************************
#     Table 2.3
#**********************************************

data <- data %>% filter(accommodation_type == "Hotel")

nrow(data)

data %>% select(hotel_id,price,distance) %>% slice(1:3)


## PART B: repeat part of the cleaning code
#using the raw csv data file
#includes some additional output
#*********************************************************

# *IMPORT AND PREPARE DATA*
  
# variables downoaded as string, often in form that is not helpful
# need to transform then to numbers that we can use

data <- read_csv(paste0(data_in_raw,"/hotelbookingdata-vienna.csv",sep=""))
# Can load from website as well
# data <- read_csv( "https://osf.io/g5dmw/download" )

# distance to center entered as string in miles with one decimal
# generate numerical variable of rating variable from string variable

data <- data %>% separate(center1distance,c("distance",NA),sep = " ") %>% 
  separate(center2distance,c("distance_alter",NA),sep = " ") %>%
  separate(accommodationtype,c(NA,"accommodation_type"),sep = "@") %>%
  separate(price_night,c(NA,NA,"nnight",NA),sep = " ") %>%
  separate(guestreviewsrating,c("rating",NA),sep = " ")
  

# check: frequency table of all values incl. missing values

tab_rating <- data %>%
  group_by(rating) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)),3))

View(tab_rating)

# check: frequency table of all values incl. missing varlues

tab_rating_reviewcount <- data %>%
  group_by(rating_reviewcount) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)),3))

View(tab_rating_reviewcount)

data <- data%>% mutate(rating_count = as.numeric(rating_reviewcount))

describe(data$rating_count)

# *RENAME VARIABLES*

data <- data %>% rename(ratingta = rating2_ta,ratingta_count = rating2_ta_reviewcount,
                country=addresscountryname,city=s_city,stars=starrating)

# look at key variables

tab_stars <- data %>%
  group_by(stars) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)),3))

View(tab_stars)

tab_rating <- data %>%
  group_by(rating) %>%
  summarise(n = n()) %>%
  mutate(percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)),3))

View(tab_rating)

#**********************************************
#     Table 2.10
#**********************************************

# Look for perfect duplicates

data <- data %>% arrange(hotel_id)

data %>% group_by(hotel_id) %>% filter(n()>1) %>% 
  select(c(hotel_id,accommodation_type,price,distance,stars,rating,rating_count))

data <- data %>% distinct()


#**********************************************
#     Missing values in text
#**********************************************

datasummary_skim(data=data,histogram=F)

data <-  data %>% mutate(misrating = ifelse(is.na(rating),1,0))

table(data$misrating)


addmargins(table(data$accommodation_type,data$misrating))

data %>% group_by(accommodation_type,misrating) %>% summarise(mean(price))

data %>% filter((misrating == 1)&(accommodation_type == "Hotel")) %>% 
  select(hotel_id, accommodation_type,price,distance, stars,rating,rating_count) %>%
  slice(1)


