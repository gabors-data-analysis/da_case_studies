########################################################################
#
# DATA ANALYSIS TEXTBOOK
# FUNDAMENTALS OF REGRESSION ANALYSIS
# ILLUSTRATION STUDY FOR CHAPTER 8
#
# data downloaded from a hotels price comparison site on October 27, 2017


# v2.2 2019-11-01 used for pix in book
# v2.3 2019-12-24 problem noted re dist
# v2.4 2020-01-24 ln distance now distance=0.05 if less than 0.05
# v2.5 2020-02-06 graphs minor edits
# v2.6 2020-03-21 graphs minor edits
# v2.7 2020-04-22 names ok
# v2.8 2020-04-30 labels edited
########################################################################
  
# WHAT THIS CODES DOES:
  
# Loads the data already cleaned and in Stata format - require haven package to import it
# Transforms price variables into logs, looks at logs regressions
# shows log regressions
# shows non-linear models such as splines
# shows some models and simulations on measurement error

# CLEAR MEMORY
rm(list=ls())

library(haven)
library(lspline)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(plyr)
library(scales)
library(tidyverse)

# CHANGE IT TO YOUR WORKING DIRECTORY
############################################################  
# SET YOUR OWN PATH HERE
############################################################  

# Sets the core parent directory
current_path = rstudioapi::getActiveDocumentContext()$path 
dir<-paste0(dirname(dirname(dirname(current_path ))),"/")


#location folders
data_in <- paste0(dir,"da_data_repo/hotels-vienna/clean/")
data_out <-  paste0(dir,"da_case_studies/ch08-hotels-non-linear/")
output <- paste0(dir,"da_case_studies/ch08-hotels-non-linear/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")


#call function
source(paste0(func, "theme_bg.R"))
# Created a helper function with some useful stuff
source(paste0(func, "da_helper_functions.R"))


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
  #save_fig("F08_1a_R", output, "small")
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
  #save_fig("F08_1b_R", output, "small")
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
  #save_fig("F08_1c_R", output, "small")
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
  #save_fig("F08_1d_R", output, "small")
  save_fig("ch08-figure-2b-hotel-loglog", output, "small")