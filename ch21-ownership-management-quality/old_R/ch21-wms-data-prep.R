######################################################################
#
# Data Analysis Textbook
# Case Study for Chapter 21 - Methods for Uncovering effects in observational data
# Data : wms-management
# Using World Management Survey
#
######################################################################
#
# What this code does:
#Data preparation

######################################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------

# install.packages("MatchIt")
# install.packages("gmodels")

library(haven)
library(tidyverse)
library(dplyr)
library(sandwich)
library(lmtest)
library(stargazer)
library(cowplot)
library(KernSmooth) # poly
library(MatchIt)
library(gmodels)

# Change working directory -------------------------------------------
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
#dir <- "/Users/zholler/Documents/Private/da_case_studies/ch21/wms-management-survey/"

#location folders
data_in <- paste0(dir,"da_data_repo/wms-management-survey/clean/")
data_out <- paste0(dir,"da_case_studies/ch21-ownership-management-quality/")
output <- paste0(dir,"da_case_studies/ch21-ownership-management-quality/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

# load ggplot theme function
source(paste0(func, "theme_bg.R"))


# Set location folders -----------------------------------------------
func <- paste0(dir, "ch00_tech_prep/")



######################################################################
#
# Part 1 - Understanding nature and patterns of data
#
######################################################################


# Load in data -------------------------------------------------------
data <- read_dta(paste(data_in,"wms_da_textbook.dta",sep=""))

# Define variables ---------------------------------------------------

# 1. Define founder/family owned and drop ownership that is missing or not relevant

# Ownership
data %>%
  group_by(ownership) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# Define foundfam owned
data$foundfam_owned <- ifelse(
       data$ownership== "Family owned, external CEO" |
       data$ownership== "Family owned, family CEO" |
       data$ownership== "Founder owned, CEO unknown" |
       data$ownership== "Founder owned, external CEO" |
       data$ownership== "Founder owned, family CEO" |
       data$ownership== "Founder owned, founder CEO" , 1, 0)

# Foundfam owned
data %>%
  group_by(foundfam_owned) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent))

# Crosstab
data %>% 
  count(ownership, foundfam_owned) %>% 
  spread(foundfam_owned, n, fill = 0)

# Describe data: observations ----------------------------------------

data %>% nrow()

# Wave - Years
data %>%
  group_by(wave) %>%
  summarise (Freq = n()) %>%
  mutate(Percent = Freq / sum(Freq) * 100, Cum = cumsum(Percent))

# Country
View( data %>%
        group_by(country) %>%
        summarise (Freq = n()) %>%
        mutate(Percent = Freq / sum(Freq)*100, Cum = cumsum(Percent)) )

# create xsec
data <- 
  data %>%
  group_by(firmid) %>%
  arrange(wave) %>%
  filter(row_number()==n()) %>% 
  ungroup()

# 2. Proportion with college degree & make bins

# Proportion of managers with a college degree
data$degree_m <- data$degree_m/100
data$degree_m <- ifelse(data$degree_m<0, NA, data$degree_m)

# Proportion of non-managers with a college degree
data$degree_nm <- data$degree_nm/100
data$degree_nm <- ifelse(data$degree_nm<0, NA, data$degree_nm)

# Generate bins from degree_nm
data$degree_nm_bins <- cut(data$degree_nm, c(0,0.001,0.05,0.20,1.01), right= F)

# Generate degree_nm_sq
data$degree_nm_sq <- data$degree_nm^2

# Crosstab
data %>% 
  group_by(degree_nm_bins) %>% 
  summarise(min = min(degree_nm),
            max = max(degree_nm), n = n())

# 3. Take log of employment
data$lnemp <- log(data$emp_firm)


# 4. Competition

data$compet_weak <- ifelse(data$competition == "   0 competitors" | data$competition == "  1-4 competitors", 1, 0)
data$compet_moder <- ifelse( data$competition == " 5-9 competitors", 1, 0)
data$compet_strong <- ifelse( data$competition == "10+ competitors", 1, 0)

# Summary
data %>% 
  group_by(competition) %>% 
  summarise(weak = max(compet_weak), 
            moder = max(compet_moder),
            strong = max(compet_strong))

data$competition <- 
  ifelse(data$compet_weak == 1, "0-4 competitors",
        ifelse(data$compet_moder == 1, "5-9 competitors", "10+ competitors"))


# 5. Industry in 2 digits

industry_names <- 
  c("food", "tobacco", "textile", "apparel", "lumber", "furniture",
    "paper", "printing", "chemical", "petrol", "rubber", "leather", "glass",
    "primary_metal", "fabricated_metal", "ind_machinery", "electronic",
    "transport", "instrument", "misc_manuf")

data$industry <- factor(data$sic, 
                        levels = sort(unique(data$sic)),
                        labels = industry_names)


# 6. Country as numeric variable (factor)

data$countrycode <- as.numeric(as.factor(data$cty))

data <- data %>% 
  mutate(age_young = firmage<30 & !is.na(firmage),
         age_old = firmage>80 & !is.na(firmage),
         age_unknown = is.na(firmage))

data$countrycode <- factor(data$countrycode)

# Sample selection ---------------------------------------------------


# Keep observations with:
#     Non-employee/Research/Gov/Other type of ownership
#     non-missing variables 
data <- data %>%
  filter(!ownership %in% c("", "Employees/COOP" , "Foundation/Research Institute", "Government", "Other" ),
         !is.na(management),
         !is.na(foundfam_owned),
         !is.na(degree_nm),
         !is.na(compet_weak),
         !is.na(compet_moder),
         !is.na(compet_strong),
         !is.na(industry), 
         !is.na(countrycode),
         !is.na(lnemp)
         )

# Summary of num. of employment
data %>%
  select(emp_firm) %>%
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
write_rds(data, paste0(data_out, "Ch21_wms_workfile_xsec.rds"))


