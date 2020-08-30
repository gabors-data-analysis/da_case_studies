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
# CH03D Distributions of Body Height and Income
# height-income-distributions  dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
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

data_in <- paste(data_dir,"height-income-distributions","clean/", sep = "/")

use_case_dir <- "ch03-distributions-height-income/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------
# load in clean and tidy data and create workfile
hrs <-  read.csv(paste(data_in,"hrs_height_income.csv", sep = "/"))

#------------------------------------------------------------------------------------------------------

hrs$rheight <- as.numeric(as.character(hrs$rheight))

# NORMAL: height of women age 55-59 
filtered_women <-  hrs %>%
  filter(age >= 55 & age < 60 & female == 1 & rheight > 1.3 & rheight < 2.1)
Hmisc::describe(hrs$rheight)


# graph --height  
ch03_normal_height <- ggplot(filtered_women, aes(x = rheight)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.025, boundary = min(filtered_women$rheight), 
                 fill = color[1], color = color.outline, alpha = 0.8, closed='left') +
  stat_function(fun = dnorm, colour= color[2],  
                args = with(filtered_women, c(mean = mean(rheight), sd = sd(rheight)))) + 
  scale_y_continuous("Density", position = "right", expand=c(0,0), limits = c(0, 6),
                     sec.axis =  sec_axis(~ . *0.025, name = "Percent",breaks =seq(0,0.15, by=0.025),labels = percent_format(accuracy = 0.1))) + 
    theme_bg() +
  xlab("Height (meters)")
ch03_normal_height
save_fig("ch03-figure-10-hist-height", output, "small")

#-------------------------------------------------------------------------------------------
# LOGNORMAL: family income of women age 55-59 

# income variable
hrs <-  hrs %>%
  mutate(income = hitot / 1000)

# filter dataset
filtered_women_income <-  hrs %>%
  filter(age >= 55 & age < 60 & female == 1 & income > 1 & income < 1000)
Hmisc::describe(filtered_women_income$income)


# graph --income 
ch03_lognormal_income <- ggplot(filtered_women_income, aes(x = income)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 20, boundary=0, closed='left', 
                 fill = color[1], color = color.outline, alpha = 0.8) +
  ylab("Percent") +   xlab("Household income (thousand USD)") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 1000), breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(expand = c(0.00,0.00),limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme_bg() 
ch03_lognormal_income
save_fig("ch03-figure-11a-hist-income", output, "small")





# ln income
filtered_women_income <- filtered_women_income %>%
  mutate(lnincome = log(income))


# graph --ln income
ch03_lognormal_lnincome <- ggplot(filtered_women_income, aes(x = lnincome)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, boundary = 0, closed='left',
                 fill = color[1], color = color.outline, alpha = 0.8) +
  stat_function(fun = dnorm, colour= color[2],  
                args = with(filtered_women_income, c(mean = mean(lnincome), sd = sd(lnincome)))) + 
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
  scale_y_continuous("Density", position = "right", expand=c(0,0), limits = c(0, 0.4),
                     sec.axis =  sec_axis(~ . *0.25, name = "Percent",breaks =seq(0,0.1, by=0.025),
                                          labels = percent_format(accuracy = 0.1))) + 
  theme_bg() +
  ylab("Percent") +   xlab("ln(household income, thousand USD)") 
  ch03_lognormal_lnincome
  save_fig("ch03-figure-11b-hist-income-log", output, "small")
  
