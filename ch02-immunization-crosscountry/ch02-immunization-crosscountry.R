################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

## Chapter 02

### CH02C Displaying immunization rates across countries
# using the world-bank-immunizationdataset
# version 0.9 2020-09-06

# CLEAR MEMORY
rm(list=ls())
library(tidyverse)
library(haven)
library(Hmisc)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

data_in_clean <- paste(data_dir,"worldbank-immunization","clean/", sep = "/")
data_in_raw <- paste(data_dir,"worldbank-immunization","raw", sep = "/")

use_case_dir <- "ch02-immunization-crosscountry/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# load in clean and tidy data and create workfile
data <- read_csv(paste0(data_in_clean,"worldbank-immunization-panel.csv",sep=""))
#data <- read_csv("https://osf.io/download/gk5cn/")
# cleaning
 
data <-  data %>% select(c(countryname,year,imm,gdppc)) %>% 
  filter((imm != 0) & (year >= 2015) & (countryname=='Pakistan'|countryname=='India'))

summary(data)

# Table 2.5
data %>% arrange(countryname, year)

#Table 2.4
data %>% pivot_wider(names_from = c(year),values_from=c(imm,gdppc))




