#********************************************************************
#  * Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.
#
# Chapter 20
# CH20B Fine tuning social media advertising
# using the ab-test-social-media dataset
# version 0.9 2020-09-13
#********************************************************************



rm(list=ls())

# packages
library(tidyverse)
library(pwr)
library(readxl)

# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"ab-test-social-media","raw", sep = "/")

use_case_dir <- "ch20-ab-test-social-media/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")

#***************************************************************
#  * PART I
#* sample size calculations
#
#* sample size calculation with planned rates

clickthrough <- 0.01
conversion <- 0.05

proportionA =  clickthrough * conversion

proportionB = proportionA * 1.2

h = 2 * asin(sqrt(proportionA)) - 2 * asin(sqrt(proportionB))

pwr.2p.test(h=h, sig.level=0.05, power = 0.8)



clickthrough <- 0.0032
conversion <- 0.0082

proportionA =  clickthrough * conversion

proportionB = proportionA * 1.2

h = 2 * asin(sqrt(proportionA)) - 2 * asin(sqrt(proportionB))

pwr.2p.test(h=h, sig.level=0.05, power = 0.8)


# Part II

# p-value of tests

data_summary <- read_excel(paste(data_in, "/ab-test-summary.xlsx",sep=""))

type_b <- 0
clicks <- c(rep(1, data_summary$clicks[1]), rep(0, data_summary[1,2]-data_summary$clicks[1]))
action <- c(rep(1, data_summary$action[1]), rep(0, data_summary[1,2]-data_summary$action[1]))
data_a <- data.frame(type_b,clicks,action)
  
type_b <- 1
clicks <- c(rep(1, data_summary$clicks[2]), rep(0, data_summary[1,2]-data_summary$clicks[2]))
action <- c(rep(1, data_summary$action[2]), rep(0, data_summary[1,2]-data_summary$action[2]))
data_b <- data.frame(type_b,clicks,action)

data = rbind(data_a,data_b)


table(data$type_b,data$clicks)

table(data$type_b,data$action)

table(data$action,data$clicks)


reg1 = lm(clicks ~ type_b,data=data)

summary(reg1)

reg2 = lm(action ~ type_b,data=data)

summary(reg2)
