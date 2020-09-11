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

# CHAPTER 22
# CH22A How does a merger between airlines affect prices?
# using the airline-tickets-usa dataset
# version 0.9 2020-09-11
#########################################################################################


###########

#
# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(tidyverse)
library(haven)
library(zoo)
library(stargazer)
library(estimatr)
library(modelsummary)
library(cowplot)

# set data dir, data used
source("set-data-directory.R")             # data_dir must be first defined 

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")
options(digits = 3)

data_in <- paste(data_dir,"airline-tickets-usa","clean/", sep = "/")
use_case_dir <- "ch22-airline-merger-prices/"


data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



# Data Preparation Steps and Descriptive Statistics

# Load in data -------------------------------------------------------



# CREATE Workfile : only before and after period

data <- read_dta(file.path(data_in, "originfinal-panel.dta"))

# ***************************************************************************
# * market = origin X final destination 
# * (note final destination is:
# * airport at end of one-way routes if 4 or fewer
# * airport in middle of return routes if there is middle & 9 or fewer



# * before = 2011 (all year)
# * after  = 2016 (all year)
# * workfile 1: drop all other years
data <- data %>%
  filter(year==2011 | year==2016)

# * create total number of passengers from shares 
# * so we can get aggreagate shares
data_agg <- data %>%
  mutate(
    ptotalAA = shareAA*passengers,
    ptotalUS = shareUS*passengers,
    ptotallargest = sharelargest*passengers
  ) %>%
  group_by(origin, finaldest, return, year) %>%
  summarise(
    airports = first(airports), 
    return_sym = first(return_sym), 
    stops = first(stops),
    ptotalAA = sum(ptotalAA), 
    ptotalUS = sum(ptotalUS),
    ptotallargest = sum(ptotallargest),
    passengers = sum(passengers),
    itinfare = sum(itinfare)
  ) %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(
    after = as.numeric(year == 2016), 
    before = as.numeric(year == 2011),
    avgprice = itinfare/passengers,
    shareAA = ptotalAA/passengers,
    shareUS = ptotalUS/passengers,
    sharelargest = ptotallargest/passengers,
    AA = as.numeric(shareAA > 0), #share variables never missing
    US = as.numeric(shareUS > 0),
    AA_and_US = as.numeric(shareAA > 0 & shareUS > 0),
    AA_or_US = as.numeric(shareAA > 0 | shareUS > 0)
  )

# create numeric ID for market
data_agg <- data_agg %>%
  arrange(origin, finaldest) %>%
  mutate(market = factor(paste(origin, finaldest, return, sep = "_")))

# passengers before and after
data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    pass_bef = mean(ifelse(before == 1, passengers, NA), na.rm = TRUE), 
    pass_aft = mean(ifelse(after == 1, passengers, NA), na.rm = TRUE)
  ) %>%
  ungroup()


# * balanced vs unbalanced part of panel
data_agg <- data_agg %>%
  group_by(market) %>%
  mutate(balanced = as.numeric(n() == 2)) %>%
  ungroup()

data_agg %>%
  group_by(balanced) %>%
  summarise(sum(passengers), n())

# Define treated and untreated markets
# treated: both AA and US present in the before period
# untreated: neither AA nor US present in the before period
# drop if only AA or only US in before period (neither treated nor untreated)

data_agg <- data_agg %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(
    treated = ifelse(balanced == 1, max(as.numeric(AA_and_US == 1 & before == 1)), NA), 
    untreated = ifelse(balanced == 1, max(as.numeric(AA_or_US == 0 & before == 1)), NA),
    smallmkt = max(as.numeric(passengers < 5000 & before == 1))
  ) %>%
  ungroup()

data_agg <- data_agg %>%
  mutate(lnavgp = ifelse(is.infinite(log(avgprice)), NA, log(avgprice))) %>%
  arrange(market, year) %>%
  group_by(market) %>%
  mutate(d_lnavgp = lnavgp - lag(lnavgp)) %>%
  ungroup()


# **************************************************************************
# * DESCRIBE
# **************************************************************************

# describe yearly data
data_agg %>%
  select(year, passengers) %>%
  group_by(year) %>%
  summarise_each(funs(N = length, 
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      q90 = quantile(., 0.90),
                      mean = mean, 
                      sum = sum)
                    )

data_agg %>%
  filter(origin=="JFK" & finaldest=="LAX") %>%
  select(market, origin, finaldest, return, year, passengers)

data_agg %>%
  filter(year == 2011) %>%
  select(smallmkt, passengers) %>%
  group_by(smallmkt) %>%
  summarise_each(funs(N = length, 
                      min = min, 
                      max = max,
                      median = median,
                      mean = mean, 
                      sum = sum))

# describe balanced
data_agg %>%
  group_by(year, balanced) %>%
  summarise(n(), sum(passengers), mean(passengers))


# describe treatment
data_agg %>%
  group_by(year, treated, untreated) %>%
  summarise(n(), sum(passengers), mean(passengers))

# describe outcome
data_agg %>%
  filter(before == 1) %>%
  select(avgprice) %>%
  summarise_all(funs(N = length, 
                      min = min, 
                      max = max,
                      median = median,
                      mean = mean, 
                      sum = sum))

data_agg %>%
  filter(avgprice==0) %>%
  select(passengers) %>%
  summarise_all(funs(N = length,
                      mean = mean, 
                      sum = sum))

write_rds(data_agg,paste0(data_out,"ch22-airline-workfile.rds"))
