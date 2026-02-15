################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 06
# CH06B CH06B Testing the likelihood of loss on a stock portfolio?
# using the sp500 dataset
# version 0.91 2021-10-21


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(arm)
library(pastecs)
library(janitor)


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/xy/Dropbox/gabors_data_analysis/da_case_studies")

# set data dir, data used
source("ch00-tech-prep/set-data-directory.R")             # data_dir must be first defined 
# alternative: give full path here, 
#            example data_dir="C:/Users/xy/Dropbox/gabors_data_analysis/da_data_repo"

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

data_in <- paste(data_dir,"sp500","clean/", sep = "/")

use_case_dir <- "ch05-stock-market-loss-generalize/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------
# LOAD  DATA
sp500 <- read_csv(paste0(data_in,"SP500_2006_16_data.csv"),na = c("", "#N/A"))
# From web
# sp500 <- read_csv("https://osf.io/download/h64z2/" , na = c("", "#N/A") )
sp500 <- subset(sp500, VALUE != "NA")


# CREATE PERCENT RETURN
sp500<- sp500 %>% 
  mutate(pct_return = (VALUE - lag(VALUE)) / lag(VALUE) * 100)


# remove first row as it has NA in pct_return
pct_return <- sp500 %>% filter(!is.na(pct_return)) %>% pull(pct_return)

sp500 <- sp500 %>%
  mutate(loss5=ifelse((pct_return < -5),1,0))

options(digits = 6)

# t-test to show p-value of two sided. One sided p-value is p/2
t.test(sp500$loss5,mu=0.01)	

