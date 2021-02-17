#install.packages("arsenal")

# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(skimr)
library(arsenal)



#################################################################
# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #
data_in <- paste(data_dir,"airbnb","clean/", sep = "/")


data <- read_csv(paste(data_in,"airbnb_london_cleaned.csv", sep = ""))
data_book <- read_csv(paste(data_in,"airbnb_london_cleaned_book.csv", sep = ""))

summary(comparedf(data, data_book))
