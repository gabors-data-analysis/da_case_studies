################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 03
# CH03 UNDER THE HOOD: MORE ON THEORETICAL DISTRIBUTIONS --- City size distribution in Japan
# city-size-japan  dataset
# version 0.9 2020-08-28



# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(scales)


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

data_in <- paste(data_dir,"city-size-japan","clean/", sep = "/")

use_case_dir <- "ch03-city-size-japan/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

#-----------------------------------------------------------------------------------------
# import data
city_size <- read_csv(paste0(data_in, "city-size-japan.csv"))
Hmisc::describe(city_size)
# create variables

city_size <-  city_size %>%
  mutate(
    pop = (pop_2015/1000),
    lnpop = log(pop)) %>%
  arrange(-pop)


city_size <- city_size %>%
  mutate (rank = seq( from = 1, to = nrow(.), by = 1))

#------------------------------------------------------------
# ln(rank) vs ln(x)

city_size <-  city_size %>%
  mutate(lnrank = log(rank))


R_03_lnrank <- ggplot(data = city_size, aes(x=lnpop, y=lnrank)) +
  geom_smooth_da(method="lm")+
  geom_point_da()+
  labs(x="ln(population)",y="ln(rank)")+
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(5, 9.5), breaks = seq(5, 9.5, by = 0.5)) +
    theme_bg() 
R_03_lnrank
#save_fig("ch03_citysize-japan-logrank", output, "small")
save_fig("ch03-figure-12-city-logrank", output, "small")


#------------------------------------------------------------
## ln P(X>x) vs ln(x) figure
## should be the same s with ln(rank) except for constant shift


# city_size <-  city_size %>%
#   mutate(P = (rank / max(nrow(.))),
#          lnP = log(P))
# 
# R_03_lnP <- ggplot(data = city_size, aes(x=lnpop, y=lnP)) +
#   geom_point_da()+
#   labs(x="ln(population)",y="")+
#   geom_smooth_da(method="lm")+
#   theme_bg()
# R_03_lnP

#---------------------------------------------------------------
# scale invariance

x1 <-  200
x2 <- 300
bound <-  0.2

print(paste0(x1, " ", x2))

city_size %>%
  filter(pop >= x1*(1-bound) & pop <= x1*(1+bound)) %>%
  count()
  
city_size %>%
  filter(pop >= x2*(1-bound) & pop <= x2*(1+bound)) %>%
  count()  

shift <-  3  
x3 <-  x1*shift
x4 <-  x2*shift

print(paste0(x3, " ", x4))

city_size %>%
  filter(pop >= x3*(1-bound) & pop <= x3*(1+bound)) %>%
  count() 

city_size %>%
  filter(pop >= x4*(1-bound) & pop <= x4*(1+bound)) %>%
  count() 






  

