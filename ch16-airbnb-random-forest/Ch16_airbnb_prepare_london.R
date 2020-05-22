############################################################
#
# DATA ANALYSIS TEXTBOOK
# MODEL SELECTION
# ILLUSTRATION STUDY
# Airbnb London 2017 march 05 data
#
#
# WHAT THIS CODES DOES:

# Transform variables and filter dataset
# Generate new features


############################################################  
# IN   airbnb_london_workfile.csv
# OUT  airbnb_london_workfile_adj.csv

library(tidyverse)
library(skimr)

# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
# dir<- "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"


#location folders
data_in <- paste0(dir,"da_data_repo/airbnb/clean/")
data_out <- paste0(dir,"da_case_studies/ch16-airbnb-random-forest/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

source(paste0(func, "theme_bg.R"))


# Import data
data <- read_csv(paste(data_in, "airbnb_london_workfile.csv", sep = ""))

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
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

write_csv(data, paste0(data_out, "airbnb_london_workfile_adj.csv"))
