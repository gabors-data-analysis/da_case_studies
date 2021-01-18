install.packages("arsenal")


# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(skimr)
library(arsenal)

source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "ch14-airbnb-reg/"

# data used
data_in <- use_case_dir

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

options(digits = 3)


area <- "hackney"
data <-
  read_csv(paste0(data_in, "airbnb_", area, "_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)

area <- "hackney"
data_old <-
  read_csv(paste0(data_in, "airbnb_", area, "_workfile_adjOLD.csv")) %>%
  mutate_if(is.character, factor)

comparedf(data, data_old)

summary(comparedf(data, data_old))

data<-data %>%
  filter(id!=16646345, id!=13893828, id!=402407) %>%
  select(-id)

skim(data_old$ln_days_since)
