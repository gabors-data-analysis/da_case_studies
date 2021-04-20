#install.packages("arsenal")

# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(skimr)
library(arsenal)
require(dplyr) 
library(compare)



#################################################################
# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #



data_in <- "/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/work/data_book/da_case_studies/ch16-airbnb-random-forest"

data <- read_csv(paste(data_in,"airbnb_london_workfile.csv", sep = "/"))
data_book <- read_csv(paste(data_in,"airbnb_london_workfile_book.csv", sep = "/"))

summary(comparedf(data,data_book))


data = subset(data, select = -c(X1, X,latitude,longitude, reviews_per_month,security_deposit,monthly_price,weekly_price,host_name, host_neighbourhood, city, street, smart_location))
  
data_book = subset(data_book, select = -c(X1,latitude,longitude,reviews_per_month,security_deposit,monthly_price,weekly_price,host_name, host_neighbourhood, city, street, smart_location))

data <- data %>% mutate_at(vars(reviews_per_month), funs(round(., 2)))

data_book <-  data_book %>% mutate_at(vars(reviews_per_month), funs(round(., 2)))



diff <-  anti_join(data,data_book)

diff

merged <-  rbind(data %>% filter(id %in% diff$id),data_book %>% filter(id %in% diff$id)) %>% arrange(id)

View(merged)

write.csv(slice(merged, 1:10),"/Users/vigadam/Dropbox/My Mac (MacBook-Air.local)/Documents/work/data_book/da_case_studies/ch14-airbnb-reg/differece2.csv")

library(xlsx)


summary(comparedf(data %>% filter(id %in% diff$id), data_book %>% filter(id %in% diff$id)))



