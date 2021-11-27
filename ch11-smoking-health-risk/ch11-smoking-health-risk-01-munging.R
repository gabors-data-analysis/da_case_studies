################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 11
# CH11 smoking
# share-health dataset
# version 0.93 2021-11-27 - revised, two files now (cleaning separate)

#########################################
# PART 1 Munging
#########################################

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Import libraries
library(haven)
library(tidyverse)
library(modelsummary)



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

data_in <- paste(data_dir,"share-health","raw/", sep = "/")
data_clean <- paste(data_dir,"share-health","clean/", sep = "/")
data_work <- paste(data_dir,"share-health","clean/work/", sep = "/")

use_case_dir <- "ch11-smoking-health-risk/"

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)
create_output_if_doesnt_exist(data_work)


#-----------------------------------------------------------------------------------------




######
# 1. PART - Create workfile

# load in raw data and save in csv. the file came from SHARE after agreemenet
share <- haven::read_stata(paste0(data_in, "easySHARE_rel6-0-0.dta"))

names(share)[names(share) == 'br015_'] <- 'br015'

share <- subset(share, select=c(mergeid, wave, country, country_mod, int_year, 
                                int_month, female, age, eduyears_mod, sphus, 
                                br015, smoking, ever_smoked, income_pct_w4, 
                                bmi, mar_stat))

write.csv(share, paste0(data_clean, "share-health.csv"), row.names = F)


# if start from here, load in clean and tidy data and create workfile
share <- read_csv(paste0(data_clean,"share-health.csv"))

# Creating binary variable for health: takes 1 if sphus is 1 or 2, otherwise 0.
share$healthy <- ifelse( share$sphus>0 & share$sphus<=5, 
                         ifelse(share$sphus==1 | share$sphus==2, 1, 0), NA)
table(share$healthy)

# Before remove observations, where healthy is missing value, check their characteristics!
# Now we are going to skip this step...
share <- share[ !is.na(share$healthy), ]

# baseline is wave 4 (2011) and the endline is wave 6 (2015)
share <- share %>% mutate( baseline = ifelse(share$wave==4, 1, 0),
                           endline  = ifelse(share$wave==6, 1, 0) )

table(share$baseline)
table(share$endline)

# We are curious, who stays healthy at endline:
#   1 if endline and healthy is 1, 0 if endline is 1, but healthy is 0, otherwise NA
share$temp <- ifelse(share$endline==1, 
                     ifelse(share$healthy==1, 1, 0), NA)
table(share$temp)

# Now we can create `stayshealthy' variable: take the maximum value for temp given mergeid
#   Needs to group by mergeid (individuals) -> to filter out duplicates
#     and check if temp is equal to 1, 0 or NA and choose the largest.
# This step takes for a while -> open sum_stat.R script!
share <- share %>% group_by( mergeid ) %>% mutate(stayshealthy = max(temp, na.rm=TRUE)) 
table(share$stayshealthy)
# Delete temporary variable
share$temp <- NULL

# keep if:
#   1) endline health outcome non-missing
#   2) baseline observations (endline outcome already defined for them)
#   3) age 50-60 at baseline
#   4) healthy individuals at baseline

share <- share %>% filter(
  stayshealthy == 1 | stayshealthy == 0,
  baseline == 1 ,
  age >= 50 & age <= 60,
  healthy == 1 )


# re-define smoking to be 0-1 (5 here means 0)
share$smoking[share$smoking == 5] <- 0
share$ever_smoked[share$ever_smoked == 5] <- 0
# keep those with non-missing observations for smoking at baseline
share <- share %>% filter( smoking == 1 | smoking == 0 ,
                           ever_smoked == 1 | ever_smoked == 0 )


##
# TO DO:
#   read 'country_id.csv' and join by country to share data
#    in the end you need to have country_str for each observations, 
#     where country_str stands for country strings

# Import
country_id <- read_csv(paste0(data_clean,"country_id.csv"))
# left join to share by country
share <- left_join( share , country_id, by = "country" )

# Check the number of people stayed healthy by countries
table(share$country_str,share$stayshealthy)

# Remove non-needed variables
rm(country_id )


# create fake ID for sharing
share= share %>%
  ungroup() %>%
  dplyr::select(-mergeid)

share = share %>% 
  mutate(ID = row_number()         ) 

share= share %>%
  relocate(ID)

###
# save ready-to-analyze data
write_csv( share, paste0(data_work , "share-health-filtered.csv") )
