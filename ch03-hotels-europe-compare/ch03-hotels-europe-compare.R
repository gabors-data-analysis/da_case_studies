################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# CHAPTER 03
# CH03B Comparing hotel prices in Europe: Vienna vs. London 	
# hotels-europe dataset
# version 0.9 2020-08-28


# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(xtable)


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

data_in <- paste(data_dir,"hotels-europe","clean/", sep = "/")

use_case_dir <- "ch03-hotels-europe-compare/"
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)


#-----------------------------------------------------------------------------------------

# load in clean and tidy data and create workfile
hotels_europe_price <- read_csv(paste0(data_in,"hotels-europe_price.csv"))
hotels_europe_features <- read_csv(paste0(data_in,"hotels-europe_features.csv"))

hotels_europe <- left_join(hotels_europe_price, hotels_europe_features, by = "hotel_id")
rm(hotels_europe_price)
rm(hotels_europe_features)

# filter for same Vienna data we used + London same date
hotels_europe_cut <- hotels_europe %>% filter(year == 2017 & month == 11 & weekend == 0) %>%
                     filter(city %in% c("Vienna", "London")) %>%
                     filter(accommodation_type == "Hotel") %>%
                     filter(stars>=3 & stars<=4) %>%
                     filter(!is.na(stars)) %>%
                     filter(city_actual %in% c("Vienna", "London")) %>%
                     filter(price <=600)


hotels_europe_cut %>%
  group_by(city) %>%
  summarise(mean_price = mean(price), max=max(price), n = n())

write_csv(hotels_europe_cut, paste0(data_out,"hotels-vienna-london.csv"))



# Vienna vs London
# have same range on x axis

histprice_Vienna5_R<- ggplot(data = filter(hotels_europe_cut, city=="Vienna"), aes (x = price)) +
  geom_histogram_da(type="percent", binwidth = 20 )+
  labs(x = "Price (US dollars)", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_y_continuous(expand = c(0.00,0.00),limits = c(0, 0.3), breaks =seq(0,0.3, by=0.1), labels = scales::percent_format()) +
  theme_bg() 
histprice_Vienna5_R
save_fig("ch03-figure-6a-hist-price-vienna", output, "small")


#  histprice_London_R<-ggplot(data = filter(hotels_europe_cut, city=="London"), aes (x = price, y = (..count..)/sum(..count..))) +
#geom_histogram(binwidth = 20, color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F, na.rm =TRUE) +

histprice_London_R<-ggplot(data = filter(hotels_europe_cut, city=="London"), aes (x = price)) +
  geom_histogram_da(type="percent", binwidth = 20 )+
  labs(x = "Price (US dollars)", y = "Percent") +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_y_continuous(expand = c(0.00,0.00),limits = c(0, 0.3), breaks =seq(0,0.3, by=0.1), labels = scales::percent_format()) +
  theme_bg() 
histprice_London_R
save_fig("ch03-figure-6b-hist-price-london", output, "small")



# kernel density plots

kdens_ViennaLondon_R<-ggplot(data = hotels_europe_cut, aes(x=price, y = stat(density), color = city)) +
  geom_line(stat="density",  show.legend=F, na.rm =TRUE) +
  labs(x="Price (US dollars)", y="Density", color = "") +
  scale_color_manual(name="", 
                     values=c(color[2],color[1]),
                     labels=c("London","Vienna")) +
  scale_y_continuous(expand = c(0.0,0.0), limits = c(0, 0.015), breaks = seq(0, 0.015, by = 0.003)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  geom_text(aes(x = 340, y = 0.0026, label = "London"), color = color[2], size=2.5) +
  geom_text(aes(x = 170, y = 0.008, label = "Vienna"), color = color[1], size=2.5) +
  theme_bg() 
kdens_ViennaLondon_R
save_fig("ch03-figure-7-kdens_ViennaLondon", output, "small")



table_3_6 <- 
  hotels_europe_cut %>%
  group_by(city) %>%
  summarise(n = length(price), mean=mean(price), median=median(price), min = min(price), max = max(price), 
            sd = sd(price), skew= ((mean(price)-median(price))/sd(price)))
table_3_6
# print out nicely
xt<-xtable(table_3_6,align='llccccccc', digits = c(0, 0,0,2,0,0,0,2,3))
names(xt) <- c('City','Observations','Mean','Median','Min','Max','Std.dev.','Skewness' )
print(xt, type = "latex", file = paste0(output,"ch03-table-6-vienna-london-compare.tex",include.rownames = FALSE))


