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

# Chapter 16
# CH16A Predicting apartment prices with random forest
# using the airbnb dataset
# version 0.9 2020-09-09
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
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

use_case_dir <- "ch16-airbnb-random-forest/"
data_in <- paste(data_dir,"airbnb","clean/", sep = "/")
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)



#-------------------------------------------------------
# Import data
data <- read_csv(paste(data_in,"airbnb_london_cleaned.csv", sep = ""))


# keep if property type is Apartment, House or Townhouse
table(data$property_type)
data <- data %>%
  filter(property_type %in% c("Apartment", "House", "Townhouse"))
# rename Townhouse to House
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Townhouse", "House", data$property_type),
    f_property_type = factor(property_type))


#Room type as factor
table(data$room_type)
data <- data %>%
  mutate(f_room_type = factor(room_type))

# Rename roomt type because it is too long
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))

# cancellation policy as factor
table(data$cancellation_policy)
# if cancellation policy is super strict 30 or 60, rename it as strict
data <- data %>%
  mutate(
    cancellation_policy = ifelse(cancellation_policy %in% c("super_strict_30", "super_strict_60"),
                                 "strict", cancellation_policy),
    f_cancellation_policy = factor(cancellation_policy))

# bed_type and neighbourhood_cleansed as factors
table(data$bed_type)
# rename to Couch
data <- data %>%
  mutate(
    bed_type = ifelse(bed_type %in% c("Futon", "Pull-out Sofa", "Airbed"), "Couch", bed_type),
    f_bed_type = factor(bed_type),
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

#---------------------------------------------------------------------------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(
    usd_price_day = price,
    p_host_response_rate = as.numeric(host_response_rate))
# rename cleaning_fee column
data <- data %>%
  rename(usd_cleaning_fee = cleaning_fee)
#-------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","guests_included",
                "reviews_per_month","extra_people","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
# create dummy vars
dummies <- names(data)[seq(73,122)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price,
         neighbourhood_cleansed,cancellation_policy,room_type,property_type)

write_csv(data, paste0(data_out, "airbnb_london_workfile.csv"))


#--------------------------------
data <- read_csv(paste(data_out,"airbnb_london_workfile.csv", sep = ""))

#####################
### look at price ###
#####################
summary(data$price)
data <- data %>%
  mutate(ln_price = log(price))
data <- data %>%
  filter(price <=1000)

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_number_of_reviews = log(n_number_of_reviews+1)
        )

# Pool accomodations with 0,1,2,10 bathrooms
data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


#------------------------------------------------------------------------------------------------


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values? 
# 1. drop if no target
data <- data %>%
  drop_na(price)


# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  ) 

# 3. drop columns when many missing not imortant
to_drop <- c("usd_cleaning_fee", "p_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
table(data$flag_n_days_since)

# redo features
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

# Look at data
datasummary_skim(data, 'numeric', histogram = TRUE)
datasummary_skim(data, 'categorical' )

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

write_csv(data, paste0(data_out, "airbnb_london_workfile_adj.csv"))
