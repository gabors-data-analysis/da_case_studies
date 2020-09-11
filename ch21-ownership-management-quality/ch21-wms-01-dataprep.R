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

# CHAPTER 21
# CH20A Founder/family ownership and quality of management
# using the wms-management dataset
# version 0.9 2020-09-11
#########################################################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------

library(tidyverse)
library(purrr)
library(haven)


# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"wms-management-survey","clean/", sep = "/")

use_case_dir <- file.path("ch21-ownership-management-quality/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)




# ***************************************************************
# *
# * PART I
# *
# * Data prep
# ***************************************************************


# Load in data -------------------------------------------------------
data <- read_csv(paste(data_in,"wms_da_textbook-xsec.csv",sep=""))

# Ownership: define founder/family owned and drop ownership that's missing or not relevant
# Ownership
data %>%
  group_by(ownership) %>%
  summarise(Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# Define foundfam owned
data$foundfam_owned <- ifelse(
       data$ownership== "Family owned, external CEO" |
       data$ownership== "Family owned, family CEO" |
       data$ownership== "Family owned, CEO unknown" |
       data$ownership== "Founder owned, external CEO" |
       data$ownership== "Founder owned, CEO unknown" |
       data$ownership== "Founder owned, founder CEO" , 1, 0)

# Foundfam owned
data %>%
  group_by(foundfam_owned) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

data %>% 
  count(ownership, foundfam_owned) %>% 
  spread(foundfam_owned, n, fill = 0)

# Proportion of managers/non-managers with a college degree
# need correction: -44 means do not know, -99 means missing
data <- data %>%
	mutate(
		degree_m = data$degree_m/100,
		degree_nm = data$degree_nm/100)  %>%
	mutate(
		degree_m = ifelse(degree_m<0, NA, degree_m),
		degree_nm = ifelse(degree_nm<0, NA, degree_nm)
	)

# Generate bins from degree_nm
quantile(data$degree_nm, na.rm = TRUE, c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1))
data$degree_nm_bins <- cut(data$degree_nm, c(0,0.001,0.05,0.20,1.01), right= FALSE)

# Generate degree_nm_sq
data$degree_nm_sq <- data$degree_nm^2

data %>% 
  group_by(degree_nm_bins) %>% 
  summarise(min = min(degree_nm),
            max = max(degree_nm), n = n())

# 3. Take log of employment
data$lnemp <- log(data$emp_firm)

# 4. Competition
table(data$competition)

#itt van valam gond 1 obs
data <- data %>%
	mutate(
		compet_weak = factor(competition == "0 competitors" | competition == "1-4 competitors"),
		compet_moder = factor(competition == "5-9 competitors"),
		compet_strong = factor( competition == "10+ competitors")
	)

data %>% 
  group_by(competition) %>% 
  summarise(weak = max(compet_weak == TRUE), 
            moder = max(compet_moder == TRUE),
            strong = max(compet_strong == TRUE))

#data$competition <- 
#  ifelse(data$compet_weak == TRUE, "0-4 competitors",
#        ifelse(data$compet_moder == TRUE, "5-9 competitors", "10+ competitors"))

# 5. Industry in 2 digits

industry_names <- 
  c("food", "tobacco", "textile", "apparel", "lumber", "furniture",
    "paper", "printing", "chemical", "petrol", "rubber", "leather", "glass",
    "primary_metal", "fabricated_metal", "ind_machinery", "electronic",
    "transport", "instrument", "misc_manuf")

data$industry <- factor(data$sic, 
                        levels = sort(unique(data$sic)),
                        labels = industry_names)

# 6. Country as factor
data$countrycode <- factor(data$cty)

# age 
data <- data %>% 
  mutate(age_young = factor(firmage<30 & !is.na(firmage)),
         age_old = factor(firmage>80 & !is.na(firmage)),
         age_unknown = factor(is.na(firmage)),
         age_mid = factor(age_young == FALSE & age_old == FALSE & age_unknown == FALSE))

# **********************************************************
# ***** SAMPLE SELECTION
# Keep observations with:
#     Non-employee/Research/Gov/Other type of ownership
#     non-missing variables 
data <- data %>%
  filter(!ownership %in% c("Government", "Other" ),
         !is.na(ownership),
  )


data <- data %>%
  filter( !is.na(management),
          !is.na(foundfam_owned),
          !is.na(degree_nm),
          !is.na(competition),
          !is.na(industry), 
          !is.na(countrycode),
          !is.na(lnemp)
  )




# Summary of num. of employment
data %>%
  dplyr::select(emp_firm) %>%
  summarise(min = min(emp_firm , na.rm=T), 
            max = max(emp_firm , na.rm=T),
            p1 = quantile(emp_firm , probs = 0.01, na.rm=T),
            p50 = quantile(emp_firm , probs = 0.50, na.rm=T),
            q99 = quantile(emp_firm , probs = 0.99, na.rm=T),
            n = n())


# Drop tiny and large firms
data %>%
  filter(emp_firm<50)  %>%
  summarise(n = n())

data %>%
  filter(emp_firm>5000)  %>%
  summarise(n = n())

data <- data %>%
  filter (!(emp_firm<50 | emp_firm>5000))


# Save workfile ------------------------------------------------------
write_rds(data, paste0(data_out, "wms_da_textbook-work.rds"))
# N=8439
