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
# CH08C Measurement error in hotel ratings
# using the hotels-vienna dataset
# version 0.9 2020-09-07
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Import libraries
library(tidyverse)
library(haven)
library(lspline)
library(grid)
library(cowplot)
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

data_in <- paste(data_dir,"hotels-vienna","clean/", sep = "/")
use_case_dir <- "ch08-hotels-measurement-error/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------
# load vienna
hotels <- read_csv(paste0(data_in,"hotels-vienna.csv"))


# ------------------------------------------------------------------------------------------------------
####SAMPLE SELECTION
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(price<=600)




#######################################
# Look at measurement error by rating count
#######################################


# TAKE LOG PRICE
hotels$lnprice <- log(hotels$price)

# define cutoffs
k1=100
k2=200

# FIGURE
reg_me <- lm(lnprice ~ rating, data=subset(hotels, rating_count<k1))
summary(reg_me)
hotels$yhat<-predict(reg_me,hotels)

reg_me2 <- lm(lnprice ~ rating, data=subset(hotels, rating_count>=k1 & rating_count<k2))
summary(reg_me2)
hotels$yhat2<-predict(reg_me2,hotels)

reg_me3 <- lm(lnprice ~ rating, data=subset(hotels, rating_count>=k2))
summary(reg_me3)
hotels$yhat3<-predict(reg_me3,hotels)


F08_noise1<- ggplot(data = hotels) +
  geom_line(aes(x = rating, y = yhat, color = color[2]), size = 1)+ 
  geom_line(aes(x = rating, y = yhat3, color = color[1]), size = 1)+ 
  scale_color_manual(name = "", values=c(color[2], color[1]), labels=NULL, guide = 'none') +
  coord_cartesian(xlim = c(2, 5), ylim = c(3.5, 5)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(2,5), breaks=seq(2,5, by=0.5)) +
  labs(x = "Average rating",y = "ln(Hotel price, US dollars)")+
  theme_bg() +
  annotate("text", x = 2.6, y = 4.4, label = "More noisy: # of ratings<100", size=2, color=color[2])+
  annotate("text", x = 3.1, y = 3.6, label = "Less noisy: # of ratings>200", size=2, color=color[1])
F08_noise1
#save_fig("F08_noise1_R", output, "small")
save_fig("ch08-figure-8-hotels-measerror", output, "small")



