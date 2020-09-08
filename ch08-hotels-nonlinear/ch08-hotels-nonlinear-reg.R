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

# Chapter 08
# CH08A Finding a good deal among hotels with nonlinear function 
# using the hotels-vienna dataset
# version 0.9 2020-09-07
#########################################################################################

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Import libraries
library(haven)
library(lspline)
library(gridExtra)
library(cowplot)
library(scales)
library(tidyverse)

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

data_in <- paste(data_dir,"hotels-vienna","clean/", sep = "/")
use_case_dir <- "ch08-hotels-nonlinear/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


# ------------------------------------------------------------------------------------------------------

# load vienna
hotels <- read_csv(paste0(data_in,"hotels-vienna.csv"))
# ------------------------------------------------------------------------------------------------------
####SAMPLE SELECTION
# Apply filters:  3-4 stars, Vienna actual, without  extreme value
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(price<=600)


#############################
#LOG MODELS
############################

# TAKE LOG PRICE
hotels$lnprice <- log(hotels$price)

hotels$distance2<-hotels$distance
hotels$distance2[hotels$distance2<0.05] <- 0.05

hotels$lndistance<-log(hotels$distance2)


# describe price and ln price
summary(hotels$price)
summary(hotels$lnprice)


# REGRESSION
reg1 <- lm(price ~ distance, data=hotels)
summary(reg1)
reg2 <- lm(price ~ lndistance, data=hotels)
summary(reg2)
reg3 <- lm(lnprice ~ distance, data=hotels)
summary(reg3)
reg4 <- lm(lnprice ~ lndistance, data=hotels)
summary(reg4)


###############
# FIGURES 8.1

# LEVEL-LEVEL LINEAR REGRESSION
F08_1a <- ggplot(data = hotels, aes(x = distance, y = price)) +
  geom_point_da() + 
  geom_smooth_da(method = "lm")+
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  labs(x = "Distance to city center (miles)",y = "Price (US dollars)")+
 theme_bg() 
  F08_1a
  save_fig("ch08-figure-1a-hotel-levlev", output, "small")

  
# LOG-LEVEL LINEAR REGRESSION 
  F08_1b <- ggplot(data = hotels, aes(x = distance, y = lnprice)) +
    geom_point_da() + 
    geom_smooth_da(method = "lm")+
    expand_limits(x = 0.01, y = 0.01) +
    scale_x_continuous(expand = c(0.01,0.01), limits=c(0, 7), breaks=seq(0, 7, by=1)) + 
    scale_y_continuous(expand = c(0.01,0.01), limits = c(3.5, 6), breaks = seq(3.5, 6, by = 0.50)) +
    labs(x = "Distance to city center (miles)",y = "ln(price, US dollars)")+
    theme_bg() 
  F08_1b
  save_fig("ch08-figure-1b-hotel-loglev", output, "small")
  
  

  # LEVEL-LOG LINEAR REGRESSION
  F08_1c <- ggplot(data = hotels, aes(x = lndistance, y = price)) +
    geom_point_da() + 
    geom_smooth_da(method = "lm")+
    expand_limits(x = 0.01, y = 0.01) +
    scale_y_continuous(expand = c(0.01,0.01), limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
    labs(x = "ln(distance to city center, miles)",y = "Price (US dollars)")+
    theme_bg() 
  F08_1c
  save_fig("ch08-figure-2a-hotel-levlog", output, "small")
  
  # LOG-LOG LINEAR REGRESSION 
  F08_1d <- ggplot(data = hotels, aes(x = lndistance, y = lnprice)) +
    geom_point_da() + 
    geom_smooth_da(method = "lm")+
    #scale_x_continuous(limits=c(-2.5, 2), breaks=seq(-2.5, 2, by=0.5)) + 
    expand_limits(x = 0.01, y = 0.01) +
    scale_y_continuous(expand = c(0.01,0.01), limits = c(3.5, 6), breaks = seq(3.5, 6, by = 0.50)) +
    labs(x = "ln(distance to city center, miles)",y = "ln(price, US dollars)")+
    theme_bg() 
  F08_1d
  save_fig("ch08-figure-2b-hotel-loglog", output, "small")
  